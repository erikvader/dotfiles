# pyright: strict

from enum import Enum, auto
from typing import Callable, Iterable, Self, NewType, Any, assert_never
from dataclasses import dataclass
from abc import ABC, abstractmethod
from inspect import signature, Parameter

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

    def __repr__(self):
        return repr((self.popped, self.tokens[::-1]))


class Assoc(Enum):
    LEFT = auto()
    RIGHT = auto()


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

    def eval(self, context: C) -> T:
        return self.func(context)

    def __repr__(self):
        return f"A[{self.token!r},{self.tokens!r}]"

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

    def eval(self, context: C) -> T:
        return self.func(context, self.left, self.right)

    def __repr__(self):
        return f"B[{self.token!r},{self.left!r},{self.right!r}]"

    def __str__(self):
        return f"({self.left} {self.token} {self.right})"


@dataclass(frozen=True)
class Unary[C, T](Tree[C, T]):
    inner: Tree[C, T]
    func: UnaryCallable[C, T]

    def eval(self, context: C) -> T:
        return self.func(context, self.inner)

    def __repr__(self):
        return f"U[{self.token!r},{self.inner!r}]"

    def __str__(self):
        return f"({self.token} {self.inner})"


@dataclass(frozen=True)
class Sub[C, T](Tree[C, T]):
    inner: Tree[Any, Any]
    func: SubCallable[C, Any, T, Any]

    def eval(self, context: C) -> T:
        return self.func(context, self.inner)

    def __repr__(self):
        return f"S[{self.token!r},{self.inner!r}]"

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


class Parser[T, C]:
    parens = None
    binaries: dict[Token, tuple[Assoc, Prec, BinaryCallable[C, T]]] = {}
    unaries: dict[Token, UnaryCallable[C, T]] = {}
    implicit_operator: Token | None = None
    atoms: dict[
        Token, tuple[int | Token, Callable[..., T], Callable[..., Iterable[Any]]]
    ] = {}
    subs: dict[Token, tuple["Parser[Any, Any]", SubCallable[C, Any, T, Any]]] = {}

    def operator(
        self, name: str, assoc: Assoc, prec: int, func: BinaryCallable[C, T]
    ) -> Self:
        assert name
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
        numargs: int | str = 0,
        mapper: ArgMapper[*As] | None = None,
    ) -> Self:
        """Add an atom.

        The argument mapper takes as many str arguments as numargs is equal to. If it
        instead is a str, then the mapper should accept a variable amount of str
        arguments. There is no way to type check this statically, so it is checked during
        runtime.

        """
        assert name

        if isinstance(numargs, str):
            tokenized = Token(numargs)
            assert tokenized
            if mapper is not None:
                sig = signature(mapper)
                assert list(p.kind for p in sig.parameters.values()) == [
                    Parameter.VAR_POSITIONAL
                ], "Mapper must only accept a single *args"
        else:
            tokenized = numargs
            assert tokenized >= 0
            if mapper is not None:
                sig = signature(mapper)
                assert all(
                    p.kind == Parameter.POSITIONAL_ONLY
                    or p.kind == Parameter.POSITIONAL_OR_KEYWORD
                    and p.default == Parameter.empty
                    for p in sig.parameters.values()
                ), "Mapper function must only take positional arguments"
                assert (
                    len(sig.parameters) == tokenized
                ), "Mapper must take the specified number of arguments"

        def noop(*args: Any) -> Iterable[Any]:
            return tuple(args)

        some_mapper = mapper if mapper is not None else noop

        self.atoms[Token(name)] = (tokenized, func, some_mapper)
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
