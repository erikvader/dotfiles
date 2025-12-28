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
from dataclasses import dataclass
from abc import ABC, abstractmethod
from inspect import signature, Parameter
from .inspecttools import shortdoc
from pathlib import Path

Token = NewType("Token", str)


class Tokens:
    def __init__(self, tokens: Iterable[str]):
        self.tokens = list(Token(t) for t in tokens)
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
        return repr((self.popped, self.tokens[::-1]))


class Assoc(Enum):
    LEFT = auto()
    RIGHT = auto()


def arg0() -> tuple[()]:
    return ()


def str1(a1: Token) -> tuple[str]:
    return (a1,)


def str2(a1: Token, a2: Token) -> tuple[str, str]:
    return (a1, a2)


def str3(a1: Token, a2: Token, a3: Token) -> tuple[str, str, str]:
    return (a1, a2, a3)


def path1(a1: Token) -> tuple[Path]:
    return (Path(a1),)


def semicolon(*args: Annotated[Token, ";"]) -> tuple[str, ...]:
    return tuple(args)


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
            commas = " ".join([self.token, *self.tokens])
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

    @override
    def eval(self, context: C) -> T:
        return self.func(context, self.inner)

    @override
    def __repr__(self):
        return f"S[{self.token!r},{self.inner!r}]"

    @override
    def __str__(self):
        return f"({self.token} {self.inner})"


@dataclass(frozen=True)
class Parens:
    left: Token
    right: Token


class ParseError(Exception):
    def __init__(self, message: str, tokens: Tokens):
        """
        The last popped token is the one that caused this exception to be raised.
        """
        super().__init__(message)
        self.tokens = tokens


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

        if self.unary_operators:
            lines.append("Unary operators:")
            for u in self.unary_operators:
                lines.append(f"  {u.name}: {u.description}")
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
                    pre = f"    TOKEN {impl}{b.name}{impr} TOKEN: "
                    lines.extend(self._append_description(pre, b.description + impd))
            lines.append("")

        if self.atoms:
            lines.append("Atoms:")
            for a in self.atoms:
                pre = f"  {a.name_with_args()}: "
                lines.extend(self._append_description(pre, a.description))
            lines.append("")

        if self.parenthesis is not None:
            lines.append("Grouping:")
            lines.append("  " + self.parenthesis)
            lines.append("")

        deferred_lines: list[str] = []
        if self.subparsers:
            lines.append("Subparsers:")
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
        self.name = name
        self.description = description

        self.parens: Parens | None = None
        self.binaries: dict[Token, tuple[Assoc, Prec, BinaryCallable[C, T]]] = {}
        self.unaries: dict[Token, UnaryCallable[C, T]] = {}
        self.implicit_operator: Token | None = None
        self.atoms: dict[
            Token, tuple[int | Token, Callable[..., T], Callable[..., Iterable[Any]]]
        ] = {}
        self.subs: dict[
            Token, tuple["Parser[Any, Any]", SubCallable[C, Any, T, Any]]
        ] = {}

    def operator(
        self, name: str, assoc: Assoc, prec: int, func: BinaryCallable[C, T]
    ) -> Self:
        assert name
        assert prec >= MIN_PREC
        self.binaries[Token(name)] = (assoc, Prec(prec), func)
        return self

    def unary(self, name: str, func: UnaryCallable[C, T]) -> Self:
        assert name
        self.unaries[Token(name)] = func
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
        assert name
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
                        numargs = Token(terminator)
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

        self.atoms[Token(name)] = (numargs, func, mapper)
        return self

    # NOTE: pylint gets confused
    # pylint: disable=redefined-outer-name
    def sub[Cp, Tp](
        self, name: str, sub_parser: "Parser[Cp, Tp]", func: SubCallable[C, Cp, T, Tp]
    ) -> Self:
        assert name
        assert sub_parser.parens is not None, "Sub parser must have parens set up"
        self.subs[Token(name)] = (sub_parser, func)
        return self

    def set_implicit(self, name: str) -> Self:
        assert name
        assert self.implicit_operator is None, "Implicit operator already set"
        self.implicit_operator = Token(name)
        assert (
            self.implicit_operator in self.binaries
        ), "Implicit operator does not exist"
        return self

    def set_parens(self, left: str, right: str) -> Self:
        assert left
        assert right
        self.parens = Parens(Token(left), Token(right))
        return self

    def parse(self, tokens: Tokens) -> Tree[C, T]:
        result = self._parse_inner(tokens, MIN_PREC)
        if not tokens.is_empty():
            raise ParseError("There are tokens left", tokens)
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
                return f"{l} TOKEN ... {r}"
            case _:
                return None

    @staticmethod
    def _meets_prec_req(test: Prec, min_prec: Prec) -> bool:
        return test >= min_prec

    def _next_operator(
        self, tokens: Tokens, min_prec: Prec
    ) -> tuple[Token, Assoc, Prec, BinaryCallable[C, T]] | None:
        if (operator_token := tokens.peek()) is None:
            return None

        if self.parens is not None and operator_token == self.parens.right:
            return None

        if self.implicit_operator is not None and operator_token not in self.binaries:
            return self._next_implicit_operator(min_prec)

        return self._next_non_implicit_operator(tokens, min_prec)

    def _next_implicit_operator(
        self, min_prec: Prec
    ) -> tuple[Token, Assoc, Prec, BinaryCallable[C, T]] | None:
        assert self.implicit_operator is not None
        assoc, prec, func = self.binaries[self.implicit_operator]
        if not Parser._meets_prec_req(prec, min_prec):
            return None

        return self.implicit_operator, assoc, prec, func

    def _next_non_implicit_operator(
        self, tokens: Tokens, min_prec: Prec
    ) -> tuple[Token, Assoc, Prec, BinaryCallable[C, T]] | None:
        operator_token = tokens.pop()
        assert operator_token is not None

        if (operator := self.binaries.get(operator_token)) is None:
            raise ParseError(f"Unknown operator '{operator_token}'", tokens)

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
        atom_token = tokens.pop()
        if atom_token is None:
            raise ParseError("Early end of tokens", tokens)

        if self.parens is not None and atom_token == self.parens.left:
            inside = self._parse_inner(tokens, MIN_PREC)
            close = tokens.pop()
            if close != self.parens.right:
                raise ParseError(
                    f"Expected a matching closing paren, but got '{close}' instead",
                    tokens,
                )
            return inside

        if (unary_func := self.unaries.get(atom_token)) is not None:
            inner = self._parse_atom(tokens)
            # NOTE: pylint doesn't understand new type parameter syntax
            # pylint: disable=too-many-function-args
            return Unary(atom_token, inner, unary_func)

        if (sub_info := self.subs.get(atom_token)) is not None:
            sub_parser, sfunc = sub_info
            assert sub_parser.parens is not None
            oppen = tokens.pop()
            if oppen != sub_parser.parens.left:
                raise ParseError(
                    f"A sub-expression must be surrounded by parens, found '{oppen}' instead",
                    tokens,
                )

            # NOTE: It's the same class accessing a member of the same class
            # pylint: disable=protected-access
            try:
                inner = sub_parser._parse_inner(tokens, MIN_PREC)
            except ParseError as pe:
                # TODO: the outer-most tokens is the same as the inner, so they will both
                # point to the same token. It would be nice if the outer pointed to
                # atom_token maybe?
                raise ParseError(
                    f"The sub-parser of '{atom_token}' failed", tokens
                ) from pe

            close = tokens.pop()
            if close != sub_parser.parens.right:
                raise ParseError(
                    f"Expected a matching closing paren, but got '{close}' instead",
                    tokens,
                )

            # NOTE: pylint doesn't understand new type parameter syntax
            # pylint: disable=too-many-function-args
            return Sub(atom_token, inner, sfunc)

        if (info := self.atoms.get(atom_token)) is None:
            raise ParseError(f"Unknown atom '{atom_token}'", tokens)

        numargs, func, mapper = info
        args: list[Token] = []

        match numargs:
            case int():
                for _ in range(numargs):
                    if (val := tokens.pop()) is None:
                        raise ParseError(
                            f"Not enough arguments for '{atom_token}'", tokens
                        )
                    args.append(val)
            case str():
                while (val := tokens.pop()) is not None:
                    if numargs == val:
                        break
                    args.append(val)
                else:
                    raise ParseError(
                        f"Could not find '{numargs}' when gathering arguments for '{atom_token}'",
                        tokens,
                    )
            case _ as unreachable:
                assert_never(unreachable)

        try:
            mapped_args = mapper(*args)
        except Exception as e:
            raise ParseError(
                f"The arguments of atom '{atom_token}' failed to map its args",
                tokens,
            ) from e

        prepped_func: Callable[[C], T] = lambda ctx: func(ctx, *mapped_args)

        # NOTE: pylint doesn't understand new type parameter syntax
        # pylint: disable=too-many-function-args
        return Atom(atom_token, args, prepped_func)
