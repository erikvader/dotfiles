# pylint: disable=redefined-outer-name

from deluge_cli.parser import (
    Tokens,
    Token,
    Parser,
    Assoc,
    Tree,
    ParseError,
    arg_semicolon,
    arg1,
    arg3,
)
from pytest import fixture, raises, mark
from typing import Any, Annotated


def test_tokens() -> None:
    tok = Tokens(["1", "2", "3"])
    assert not tok.is_empty()
    assert tok.peek() == "1"
    assert tok.peek() == "1"
    assert tok.pop() == "1"
    assert tok.pop() == "2"
    assert tok.pop() == "3"
    tok.unpop()
    assert tok.pop() == "3"
    assert tok.pop() is None
    assert tok.peek() is None
    assert tok.is_empty()


@fixture
def simple_math_parser() -> Parser[None, int]:
    def number_func(num: int) -> Any:
        return lambda *_rest: num

    def add_func(ctx: None, left: Tree, right: Tree) -> int:
        return left(ctx) + right(ctx)

    def multiply_func(ctx: None, left: Tree, right: Tree) -> int:
        return left(ctx) * right(ctx)

    parser = (
        Parser()
        .atom("0", number_func(0))
        .atom("1", number_func(1))
        .atom("2", number_func(2))
        .atom("3", number_func(3))
        .atom("4", number_func(4))
        .operator("+", Assoc.LEFT, 1, add_func)
        .operator("*", Assoc.RIGHT, 2, multiply_func)
        .set_parens("(", ")")
    )

    return parser


def test_single_atom(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("1")
    parsed = simple_math_parser.parse(tokens)
    assert parsed(None) == 1
    assert str(parsed) == "1"


def test_empty(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("")
    with raises(ParseError):
        simple_math_parser.parse(tokens)


def test_single_operator_missing_right(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("1 +")
    with raises(ParseError):
        simple_math_parser.parse(tokens)


def test_only_operator(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("+")
    with raises(ParseError):
        simple_math_parser.parse(tokens)


def test_single_operator(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("1 + 2")
    parsed = simple_math_parser.parse(tokens)
    assert parsed(None) == 3
    assert str(parsed) == "(1 + 2)"


def test_double_operator_left_assoc(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("1 + 2 + 3")
    parsed = simple_math_parser.parse(tokens)
    assert parsed(None) == 6
    assert str(parsed) == "((1 + 2) + 3)"


def test_double_operator_right_assoc(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("1 * 2 * 3")
    parsed = simple_math_parser.parse(tokens)
    assert parsed(None) == 6
    assert str(parsed) == "(1 * (2 * 3))"


def test_double_operator_mixed(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("1 + 2 * 3")
    parsed = simple_math_parser.parse(tokens)
    assert str(parsed) == "(1 + (2 * 3))"
    assert parsed(None) == 7

    tokens = Tokens.split("1 * 2 + 3")
    parsed = simple_math_parser.parse(tokens)
    assert str(parsed) == "((1 * 2) + 3)"
    assert parsed(None) == 5


def test_parens(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("( 1 + 2 ) * 3")
    parsed = simple_math_parser.parse(tokens)
    assert str(parsed) == "((1 + 2) * 3)"
    assert parsed(None) == 9


def test_parens_righty(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("1 + ( 2 + ( 3 + 4 ) )")
    parsed = simple_math_parser.parse(tokens)
    assert str(parsed) == "(1 + (2 + (3 + 4)))"
    assert parsed(None) == 10


def test_missing_closing_parens(simple_math_parser: Parser) -> None:
    tokens = Tokens.split("1 + ( 2 + ( 3 + 4 )")
    with raises(ParseError):
        simple_math_parser.parse(tokens)


@fixture
def multi_arg_parser() -> Parser[list[int], None]:
    def op_func(num: int):
        return lambda ctx, *_rest: ctx.append(num)

    def mapper(arg: Token) -> tuple[int]:
        if arg != "omg":
            raise ValueError("arg is not omg")
        return (1,)

    def mapper_var(*_arg: Annotated[Token, ";"]) -> tuple[int]:
        return (2,)

    def int_func(ctx: list[int], num: int) -> None:
        ctx.append(num)

    parser = (
        Parser()
        .atom("op0", op_func(0))
        .atom("op1", op_func(1), arg1)
        .atom("op3", op_func(3), arg3)
        .atom("op4", op_func(4), arg_semicolon)
        .atom("op5", int_func, mapper)
        .atom("op6", int_func, mapper_var)
    )

    return parser


def test_op_args(multi_arg_parser: Parser) -> None:
    parsed = multi_arg_parser.parse(Tokens.split("op1 arg1"))
    assert str(parsed) == "(op1 arg1)"

    with raises(ParseError):
        multi_arg_parser.parse(Tokens.split("op1 arg1 arg2"))


def test_op_args3(multi_arg_parser: Parser) -> None:
    with raises(ParseError):
        multi_arg_parser.parse(Tokens.split("op3 arg1 arg2"))

    parsed = multi_arg_parser.parse(Tokens.split("op3 1 2 3"))
    assert str(parsed) == "(op3 1 2 3)"


def test_varargs_op(multi_arg_parser: Parser) -> None:
    with raises(ParseError):
        multi_arg_parser.parse(Tokens.split("op4 1"))

    parsed = multi_arg_parser.parse(Tokens.split("op4 1 2 3 ;"))
    assert str(parsed) == "(op4 1 2 3)"

    parsed = multi_arg_parser.parse(Tokens.split("op4 ;"))
    assert str(parsed) == "op4"


def test_mapper(multi_arg_parser: Parser) -> None:
    with raises(ParseError):
        multi_arg_parser.parse(Tokens.split("op5 asd"))

    parsed = multi_arg_parser.parse(Tokens.split("op5 omg"))
    assert str(parsed) == "(op5 omg)"

    order = []
    assert parsed(order) is None
    assert order == [1]


@fixture
def bool_parser() -> Parser:
    def true_func(ctx: list[int], arg: str, *rest: str) -> bool:
        assert not rest
        ctx.append(int(arg))
        return True

    def false_func(ctx: list[int], arg: str, *rest: str) -> bool:
        assert not rest
        ctx.append(int(arg))
        return False

    def or_func(ctx: list[str], left: Tree, right: Tree) -> bool:
        return left(ctx) or right(ctx)

    def and_func(ctx: list[str], left: Tree, right: Tree) -> bool:
        return left(ctx) and right(ctx)

    parser = (
        Parser()
        .atom("true", true_func, arg1)
        .atom("false", false_func, arg1)
        .operator("and", Assoc.LEFT, 2, and_func)
        .operator("or", Assoc.LEFT, 1, or_func)
        .set_parens("(", ")")
    )

    return parser


@mark.parametrize(
    "inp,res,odr",
    [
        ("true 1 and true 2", True, [1, 2]),
        ("true 1 and true 2 and true 3 and true 4", True, [1, 2, 3, 4]),
        ("true 1 and true 2 and false 3 and true 4", False, [1, 2, 3]),
        ("false 1 and true 2 and false 3 and true 4", False, [1]),
        ("true 1 or true 2 or true 3", True, [1]),
        ("false 1 or true 2 or true 3", True, [1, 2]),
        ("false 1 or true 2 and false 3", False, [1, 2, 3]),
        ("false 1 or false 2 and true 3", False, [1, 2]),
        ("( true 1 or false 2 ) and true 3", True, [1, 3]),
    ],
)
def test_bool_parser(bool_parser: Parser, inp: str, res: bool, odr: list[int]) -> None:
    parsed = bool_parser.parse(Tokens.split(inp))
    order = []
    assert parsed(order) == res
    assert order == odr


@fixture
def bool_parser_implicit(bool_parser: Parser) -> Parser:
    bool_parser.set_implicit("and")
    return bool_parser


def test_implicit_and(bool_parser_implicit: Parser) -> None:
    parsed = bool_parser_implicit.parse(Tokens.split("true 1 true 2"))
    assert str(parsed) == "((true 1) and (true 2))"


def test_eval_order_parens_implicit(bool_parser_implicit: Parser) -> None:
    parsed = bool_parser_implicit.parse(Tokens.split("( true 1 or false 2 ) true 3"))
    order = []
    assert parsed(order)
    assert order == [1, 3]


@fixture
def bool_parser_not(bool_parser: Parser) -> Parser:
    def not_func(ctx: list[str], inner: Tree) -> bool:
        return not inner(ctx)

    bool_parser.unary("not", not_func)
    return bool_parser


def test_boolean_not(bool_parser_not: Parser) -> None:
    parsed = bool_parser_not.parse(Tokens.split("not true 1"))
    order = []
    assert not parsed(order)
    assert order == [1]


def test_boolean_not_not(bool_parser_not: Parser) -> None:
    parsed = bool_parser_not.parse(Tokens.split("not not true 1"))
    order = []
    assert parsed(order)
    assert order == [1]
    assert str(parsed) == "(not (not (true 1)))"


def test_boolean_not_longer(bool_parser_not: Parser) -> None:
    parsed = bool_parser_not.parse(Tokens.split("true 1 and not true 2 or true 3"))
    order = []
    assert parsed(order)
    assert order == [1, 2, 3]
    assert str(parsed) == "(((true 1) and (not (true 2))) or (true 3))"


@fixture
def bool_sub_parser(bool_parser_not: Parser, simple_math_parser: Parser) -> Parser:
    def calc_func(ctx: list[str], math: Tree[None, int]) -> bool:
        return math(None) == len(ctx)

    bool_parser_not.sub("calc", simple_math_parser, calc_func)
    return bool_parser_not


def test_sub_parser(bool_sub_parser: Parser) -> None:
    parsed = bool_sub_parser.parse(Tokens.split("true 1 and calc ( 1 + 0 )"))
    order = []
    assert parsed(order)
    assert order == [1]
    assert str(parsed) == "((true 1) and (calc (1 + 0)))"
