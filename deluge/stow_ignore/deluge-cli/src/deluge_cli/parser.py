# pyright: strict

import textwrap
from enum import Enum, auto
from itertools import groupby
from typing import (
    Callable,
    Iterable,
    Self,
    NewType,
    Any,
    assert_never,
    Annotated,
    get_origin,
    get_args,
    override,
)
from dataclasses import dataclass, replace
from abc import ABC, abstractmethod
from inspect import signature, Parameter
from .inspecttools import shortdoc
from pathlib import Path


@dataclass(frozen=True)
class Token:
    name: str
    position: int | None

    @classmethod
    def injected(cls, name: str) -> Self:
        return cls(name, None)

    def __repr__(self) -> str:
        return f"{self.name}[{self.position}]"

    def __str__(self) -> str:
        return self.name


class Tokens:
    def __init__(self, tokens: Iterable[str]):
        self.tokens = list(Token(t, i) for i, t in enumerate(tokens))
        self.tokens.reverse()
        self.popped: list[Token] = []

    @classmethod
    def split(cls, inp: str) -> Self:
        return cls(inp.split())

    def pop(self) -> Token | None:
        if self.is_empty():
            return None
        self.popped.append(popped := self.tokens.pop())
        return popped

    def unpop(self):
        assert self.popped, "invalid to unpop nothing"
        self.tokens.append(self.popped.pop())

    def peek(self) -> Token | None:
        if self.is_empty():
            return None
        return self.tokens[-1]

    def is_empty(self) -> bool:
        return not self.tokens

    @override
    def __repr__(self):
        return repr(
            ([str(t) for t in self.popped], [str(t) for t in self.tokens[::-1]])
        )


class Assoc(Enum):
    LEFT = auto()
    RIGHT = auto()


def arg0() -> tuple[()]:
    return ()


def str1(a1: Token) -> tuple[str]:
    return (a1.name,)


def str2(a1: Token, a2: Token) -> tuple[str, str]:
    return (a1.name, a2.name)


def str3(a1: Token, a2: Token, a3: Token) -> tuple[str, str, str]:
    return (a1.name, a2.name, a3.name)


def path1(a1: Token) -> tuple[Path]:
    return (Path(a1.name),)


def semicolon(*args: Annotated[Token, ";"]) -> tuple[str, ...]:
    return tuple(a.name for a in args)


# NOTE: there is no way to specify that the number of arguments must be the same as the
# tuple length, so it is left as a ...
type ArgMapper[*As] = Callable[..., tuple[*As]]
type AtomCallable[C, T, *As] = Callable[[C, *As], T]
type BinaryCallable[C, T] = Callable[[C, Tree[C, T], Tree[C, T]], T]
type UnaryCallable[C, T] = Callable[[C, Tree[C, T]], T]
type SubCallable[C, Cp, T, Tp] = Callable[[C, Tree[Cp, Tp]], T]

Prec = NewType("Prec", int)
MIN_PREC = Prec(0)


@dataclass(frozen=True)
class Tree[C, T](ABC):
    token: Token

    def name(self) -> str:
        return self.token.name

    @abstractmethod
    def eval(self, context: C) -> T: ...

    def __call__(self, context: C) -> T:
        return self.eval(context)


@dataclass(frozen=True)
class Atom[C, T](Tree[C, T]):
    tokens: list[Token]
    func: Callable[[C], T]

    @override
    def eval(self, context: C) -> T:
        return self.func(context)

    @override
    def __repr__(self):
        return f"A[{self.token!r},{self.tokens!r}]"

    @override
    def __str__(self):
        if self.tokens:
            commas = " ".join(str(t) for t in [self.token, *self.tokens])
            return f"({commas})"
        return f"{self.token}"


@dataclass(frozen=True)
class Binary[C, T](Tree[C, T]):
    left: Tree[C, T]
    right: Tree[C, T]
    func: BinaryCallable[C, T]

    @override
    def eval(self, context: C) -> T:
        return self.func(context, self.left, self.right)

    @override
    def __repr__(self):
        return f"B[{self.token!r},{self.left!r},{self.right!r}]"

    @override
    def __str__(self):
        return f"({self.left} {self.token} {self.right})"


@dataclass(frozen=True)
class Unary[C, T](Tree[C, T]):
    inner: Tree[C, T]
    func: UnaryCallable[C, T]

    @override
    def eval(self, context: C) -> T:
        return self.func(context, self.inner)

    @override
    def __repr__(self):
        return f"U[{self.token!r},{self.inner!r}]"

    @override
    def __str__(self):
        return f"({self.token} {self.inner})"


@dataclass(frozen=True)
class Sub[C, T](Tree[C, T]):
    inner: Tree[Any, Any]
    func: SubCallable[C, Any, T, Any]

    def change_callable(self, new_func: SubCallable[C, Any, T, Any]) -> Self:
        if signature(self.func) != signature(new_func):
            raise TypeError("The function signatures are not the same")
        return replace(self, func=new_func)

    @override
    def eval(self, context: C) -> T:
        return self.func(context, self.inner)

    @override
    def __repr__(self):
        return f"S[{self.token!r},{self.inner!r}]"

    @override
    def __str__(self):
        return f"({self.token} {self.inner})"


type Visitor = Callable[[Tree[Any, Any]], None]


def visit_default(visitor: Visitor, tree: Tree[Any, Any]):
    match tree:
        case Unary():
            visitor(tree.inner)
        case Binary():
            visitor(tree.left)
            visitor(tree.right)
        case Sub():
            visitor(tree.inner)
        case _:
            pass


@dataclass(frozen=True)
class Parens:
    left: str
    right: str


class ParseError(Exception):
    def __init__(self, message: str, token: Token):
        super().__init__(message)
        self.token = token

    def __str__(self) -> str:
        return f"On token '{self.token.name}' at {self.token.position}: {super().__str__()}"


@dataclass(frozen=True)
class BinaryDoc:
    name: str
    associativity: str
    description: str
    is_implicit: bool


@dataclass(frozen=True)
class UnaryDoc:
    name: str
    description: str


@dataclass(frozen=True)
class AtomDoc:
    name: str
    args: str
    description: str

    def name_with_args(self) -> str:
        return " ".join(n for n in [self.name, self.args] if n)


@dataclass(frozen=True)
class SubDoc:
    name: str
    args: str
    description: str
    subparser: "ParserDoc"


@dataclass(frozen=True)
class ParserDoc:
    name: str
    description: str
    parenthesis: str | None
    atoms: list[AtomDoc]
    subparsers: list[SubDoc]
    unary_operators: list[UnaryDoc]
    binary_operators: list[tuple[int, list[BinaryDoc]]]

    @staticmethod
    def _append_description(prefix: str, desc: str) -> list[str]:
        return textwrap.wrap(prefix + desc, subsequent_indent=" " * len(prefix))

    def _str_lines(self) -> list[str]:
        lines = ["=" * 5 + self.name + "=" * 5]

        lines.extend(textwrap.wrap(self.description))
        lines.append("")

        lines.append("All tokens are parsed into one expression:")
        lines.append("  EXPR = ATOM | EXPR BINOP EXPR | UNOP ATOM")
        lines.append("")

        if self.unary_operators:
            lines.append("Unary operators:")
            for u in self.unary_operators:
                lines.append(f"  {u.name} ATOM: {u.description}")
            lines.append("")

        if self.binary_operators:
            lines.append("Binary operators:")
            for prec, bs in self.binary_operators:
                lines.append(f"  Precedence {prec}")
                for b in bs:
                    if b.is_implicit:
                        impl, impr, impd = (
                            "[",
                            "]",
                            " This can be omitted and is implicitly assumed if no operator is given between two atoms.",
                        )
                    else:
                        impl, impr, impd = ("", "", "")
                    pre = f"    EXPR {impl}{b.name}{impr} EXPR: "
                    lines.extend(self._append_description(pre, b.description + impd))
            lines.append("")

        if self.atoms:
            lines.append("Atoms:")
            for a in self.atoms:
                pre = f"  {a.name_with_args()}: "
                lines.extend(self._append_description(pre, a.description))

            if self.parenthesis is not None:
                lines.append(
                    "  " + self.parenthesis + ": Grouping, turn an EXPR into an ATOM"
                )

            lines.append("")

        deferred_lines: list[str] = []
        if self.subparsers:
            lines.append("Subparsers (atom):")
            for s in self.subparsers:
                pre = f"  {s.name} {s.args}: "
                lines.extend(self._append_description(pre, s.description))
                # NOTE: it's its own class...
                # pylint: disable=protected-access
                deferred_lines += s.subparser._str_lines()
            lines.append("")

        return lines + deferred_lines

    def __str__(self) -> str:
        return "\n".join(self._str_lines())


class Parser[C, T]:
    def __init__(
        self,
        *,
        name: str = "Unknown",
        description: str = "",
    ):
        if not name:
            raise ValueError("name must be non-empty")
        self.name = name
        self.description = description

        self.parens: Parens | None = None
        self.binaries: dict[str, tuple[Assoc, Prec, BinaryCallable[C, T]]] = {}
        self.unaries: dict[str, UnaryCallable[C, T]] = {}
        self.implicit_operator: str | None = None
        self.atoms: dict[
            str, tuple[int | str, Callable[..., T], Callable[..., Iterable[Any]]]
        ] = {}
        self.subs: dict[str, tuple["Parser[Any, Any]", SubCallable[C, Any, T, Any]]] = (
            {}
        )
        self.verifiers: list[Visitor] = []

    def operator(
        self, name: str, assoc: Assoc, prec: int, func: BinaryCallable[C, T]
    ) -> Self:
        if not name:
            raise ValueError("name must be non-empty")
        if prec < MIN_PREC:
            raise ValueError("precedence is smaller than the minimum")
        if name in self.binaries:
            raise ValueError(f"{name} already exists")
        self.binaries[name] = (assoc, Prec(prec), func)
        return self

    def unary(self, name: str, func: UnaryCallable[C, T]) -> Self:
        if not name:
            raise ValueError("name must be non-empty")
        if name in self.unaries:
            raise ValueError(f"{name} already exists")
        self.unaries[name] = func
        return self

    # NOTE: pylint gets confused
    # pylint: disable=redefined-outer-name
    def atom[*As](
        self,
        name: str,
        func: AtomCallable[C, T, *As],
        mapper: ArgMapper[*As] = arg0,
    ) -> Self:
        """Add an atom."""
        if not name:
            raise ValueError("name must be non-empty")
        sig = signature(mapper)
        match list(sig.parameters.values()):
            case [p] if p.kind == Parameter.VAR_POSITIONAL:
                typ = p.annotation
                if get_origin(typ) is not Annotated:
                    raise ValueError(
                        f"A single varargs must be annotated with end token: {p}"
                    )

                match get_args(typ):
                    case [contained, terminator]:
                        if contained is not Token:
                            raise ValueError(f"The type should be Token: {typ}")
                        if not isinstance(terminator, str):
                            raise ValueError(f"Terminator should be a str: {typ}")
                        numargs = terminator
                    case _:
                        raise ValueError(f"Invalid Annotated: {typ}")
            case [*ps] if all(
                p.kind == Parameter.POSITIONAL_ONLY
                or p.kind == Parameter.POSITIONAL_OR_KEYWORD
                and p.default == Parameter.empty
                for p in ps
            ):
                if not all(p.annotation is Token for p in ps):
                    raise ValueError(f"All params should have type Token: {ps}")
                numargs = len(ps)
            case _:
                raise ValueError(
                    f"Not a valid mapper signature in terms of number of parameters: {mapper}"
                )

        if name in self.atoms:
            raise ValueError(f"{name} already exists")
        self.atoms[name] = (numargs, func, mapper)
        return self

    # NOTE: pylint gets confused
    # pylint: disable=redefined-outer-name
    def sub[Cp, Tp](
        self, name: str, sub_parser: "Parser[Cp, Tp]", func: SubCallable[C, Cp, T, Tp]
    ) -> Self:
        if not name:
            raise ValueError("name must be non-empty")
        if sub_parser.parens is None:
            raise ValueError("Sub parser must have parens set up")
        if name in self.subs:
            raise ValueError(f"{name} already exists")
        self.subs[name] = (sub_parser, func)
        return self

    def add_verifier(self, visitor: Visitor) -> Self:
        self.verifiers.append(visitor)
        return self

    def set_implicit(self, name: str) -> Self:
        if not name:
            raise ValueError("name must be non-empty")
        if self.implicit_operator is not None:
            raise ValueError("Implicit operator already set")
        if name not in self.binaries:
            raise ValueError("Implicit operator does not exist")
        self.implicit_operator = name
        return self

    def set_parens(self, left: str, right: str) -> Self:
        if not left or not right:
            raise ValueError("Both halves must be non-empty")
        self.parens = Parens(left, right)
        return self

    def parse(self, tokens: Tokens) -> Tree[C, T]:
        result = self._parse_inner(tokens, MIN_PREC)

        if (peek := tokens.peek()) is not None:
            raise ParseError("There are tokens left", peek)

        for ver in self.verifiers:
            ver(result)

        return result

    def docs(self) -> ParserDoc:
        return ParserDoc(
            name=self.name,
            description=self.description,
            atoms=self._atoms_doc(),
            subparsers=self._subs_doc(),
            parenthesis=self._parens_doc(),
            binary_operators=self._binaries_doc(),
            unary_operators=self._unaries_doc(),
        )

    def _binaries_doc(self) -> list[tuple[int, list[BinaryDoc]]]:
        flat = [
            (token, assoc, prec, call)
            for token, (assoc, prec, call) in self.binaries.items()
        ]
        key: Callable[[tuple[Any, Any, Prec, Any]], int] = lambda x: x[2]
        flat.sort(key=key)
        return [
            (
                p,
                [
                    BinaryDoc(
                        name=token,
                        description=shortdoc(call),
                        associativity="left" if assoc == Assoc.LEFT else "right",
                        is_implicit=token == self.implicit_operator,
                    )
                    for (token, assoc, _, call) in xs
                ],
            )
            for p, xs in groupby(flat, key=key)
        ]

    def _subs_doc(self) -> list[SubDoc]:
        def arg(parser: Parser[Any, Any]) -> str:
            parens = parser.parens
            assert parens is not None
            return f"{parens.left} {parser.name}.TOKEN ... {parens.right}"

        return [
            SubDoc(
                name=token,
                description=shortdoc(call),
                args=arg(sub),
                subparser=sub.docs(),
            )
            for token, (sub, call) in self.subs.items()
        ]

    def _atoms_doc(self) -> list[AtomDoc]:
        def arg(numargs: int | str) -> str:
            match numargs:
                case str():
                    return "ARG1 ... ARG"
                case 0:
                    return ""
                case 1:
                    # TODO: use the argument names of the AtomCallable instead of ARG
                    return "ARG"
                case 2:
                    return "ARG1 ARG2"
                case 3:
                    return "ARG1 ARG2 ARG3"
                case n:
                    return f"ARG1 ... ARG{n}"

        return [
            AtomDoc(name=token, args=arg(numargs), description=shortdoc(call))
            for token, (numargs, call, _) in self.atoms.items()
        ]

    def _unaries_doc(self) -> list[UnaryDoc]:
        return [
            UnaryDoc(name=token, description=shortdoc(call))
            for token, call in self.unaries.items()
        ]

    def _parens_doc(self) -> None | str:
        match self.parens:
            case Parens(l, r):
                return f"{l} EXPR {r}"
            case _:
                return None

    @staticmethod
    def _expect(token: Token | None) -> Token:
        if token is None:
            raise ParseError("Early end of tokens", Token.injected("EOF"))
        return token

    @staticmethod
    def _meets_prec_req(test: Prec, min_prec: Prec) -> bool:
        return test >= min_prec

    def _next_operator(
        self, tokens: Tokens, min_prec: Prec
    ) -> tuple[Token, Assoc, Prec, BinaryCallable[C, T]] | None:
        if (operator_token := tokens.peek()) is None:
            return None

        if self.parens is not None and operator_token.name == self.parens.right:
            return None

        if (
            self.implicit_operator is not None
            and operator_token.name not in self.binaries
        ):
            return self._next_implicit_operator(min_prec)

        return self._next_non_implicit_operator(tokens, min_prec)

    def _next_implicit_operator(
        self, min_prec: Prec
    ) -> tuple[Token, Assoc, Prec, BinaryCallable[C, T]] | None:
        assert self.implicit_operator is not None
        assoc, prec, func = self.binaries[self.implicit_operator]
        if not Parser._meets_prec_req(prec, min_prec):
            return None

        return Token.injected(self.implicit_operator), assoc, prec, func

    def _next_non_implicit_operator(
        self, tokens: Tokens, min_prec: Prec
    ) -> tuple[Token, Assoc, Prec, BinaryCallable[C, T]] | None:
        operator_token = tokens.pop()
        assert operator_token is not None

        if (operator := self.binaries.get(operator_token.name)) is None:
            raise ParseError(f"Unknown operator '{operator_token}'", operator_token)

        assoc, prec, func = operator
        if not Parser._meets_prec_req(prec, min_prec):
            tokens.unpop()
            return None

        return operator_token, assoc, prec, func

    def _parse_inner(self, tokens: Tokens, min_prec: Prec) -> Tree[C, T]:
        result: Tree[C, T] = self._parse_atom(tokens)

        while (operator := self._next_operator(tokens, min_prec)) is not None:
            operator_token, assoc, prec, func = operator
            next_prec = Prec(prec + 1) if assoc is Assoc.LEFT else prec
            rhs = self._parse_inner(tokens, next_prec)
            # NOTE: pylint doesn't understand new type parameter syntax
            # pylint: disable=too-many-function-args
            result = Binary(operator_token, result, rhs, func)

        return result

    def _parse_atom(self, tokens: Tokens) -> Tree[C, T]:
        atom_token = self._expect(tokens.pop())

        if self.parens is not None and atom_token.name == self.parens.left:
            inside = self._parse_inner(tokens, MIN_PREC)
            close = self._expect(tokens.pop())
            if close.name != self.parens.right:
                raise ParseError(
                    f"Expected a matching closing paren, but got '{close}' instead",
                    close,
                )
            return inside

        if (unary_func := self.unaries.get(atom_token.name)) is not None:
            inner = self._parse_atom(tokens)
            # NOTE: pylint doesn't understand new type parameter syntax
            # pylint: disable=too-many-function-args
            return Unary(atom_token, inner, unary_func)

        if (sub_info := self.subs.get(atom_token.name)) is not None:
            sub_parser, sfunc = sub_info
            assert sub_parser.parens is not None
            oppen = self._expect(tokens.pop())
            if oppen.name != sub_parser.parens.left:
                raise ParseError(
                    f"A sub-expression must be surrounded by parens, found '{oppen}' instead",
                    oppen,
                )

            # NOTE: It's the same class accessing a member of the same class
            # pylint: disable=protected-access
            try:
                inner = sub_parser._parse_inner(tokens, MIN_PREC)
            except ParseError as pe:
                raise ParseError(
                    f"The sub-parser of '{atom_token}' failed", atom_token
                ) from pe

            close = self._expect(tokens.pop())
            if close.name != sub_parser.parens.right:
                raise ParseError(
                    f"Expected a matching closing paren, but got '{close}' instead",
                    close,
                )

            # NOTE: pylint doesn't understand new type parameter syntax
            # pylint: disable=too-many-function-args
            return Sub(atom_token, inner, sfunc)

        if (info := self.atoms.get(atom_token.name)) is None:
            raise ParseError(f"Unknown atom '{atom_token}'", atom_token)

        numargs, func, mapper = info
        args: list[Token] = []

        match numargs:
            case int():
                for _ in range(numargs):
                    if (val := tokens.pop()) is None:
                        raise ParseError(
                            f"Not enough arguments for '{atom_token}'", atom_token
                        )
                    args.append(val)
            case str():
                while (val := tokens.pop()) is not None:
                    if numargs == val.name:
                        break
                    args.append(val)
                else:
                    raise ParseError(
                        f"Could not find '{numargs}' when gathering arguments for '{atom_token}'",
                        atom_token,
                    )
            case _ as unreachable:
                assert_never(unreachable)

        try:
            mapped_args = mapper(*args)
        except Exception as e:
            raise ParseError(
                f"The arguments of atom '{atom_token}' failed to map its args",
                atom_token,
            ) from e

        prepped_func: Callable[[C], T] = lambda ctx: func(ctx, *mapped_args)

        # NOTE: pylint doesn't understand new type parameter syntax
        # pylint: disable=too-many-function-args
        return Atom(atom_token, args, prepped_func)
