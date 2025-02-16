package parser

import "core:fmt"
import "core:mem"
import "core:slice"
import "core:strconv"
import s "core:strings"
import u "core:unicode"
import "core:unicode/utf8"

TokenKind :: enum u8 {
	EOF,
	_AtomBegin,
	Ident,
	Integer,
	Float,
	_AtomEnd,
	_OpBegin,
	_BinaryBegin,
	Add,
	Sub,
	Mul,
	Div,
	Factorial,
	LSquareB,
	Exp,
	Eq,
	Compose,
	_BinaryEnd,
	_TernaryBegin,
	QuestionMark,
	Colon,
	_TernaryEnd,
	_UnaryBegin,
	UnarySub,
	LPar,
	UnaryAdd,
	_UnaryEnd,
	RSquareB,
	RPar,
	_OpEnd,
	COUNT,
}

Token :: struct {
	text: string,
	kind: TokenKind,
}

Ident :: struct {
	name: string,
}

Integer :: struct {
	value: int,
}

Float :: struct {
	value: f32,
}

BinaryExpr :: struct {
	l: ^Expr,
	r: ^Expr,
}

TernaryExpr :: struct {
	l: ^Expr,
	m: ^Expr,
	r: ^Expr,
}

UnaryExpr :: struct {
	e: ^Expr,
}

// size: 32b since largest case (TernaryExpr) is 24 and there needs to be room for tag
// Could have just used a union but I wanted to try to keep the size small
// Kind of required the hack of using op: TokenKind in place of the tag
Expr :: struct {
	operands: struct #raw_union {
		ident:        Ident,
		integer:      Integer,
		float:      Float,
		binary_expr:  BinaryExpr,
		unary_expr:   UnaryExpr,
		ternary_expr: TernaryExpr,
	},
	op:       TokenKind,
}

Lexer :: struct {
    current: int,
	tokens: [dynamic]Token,
}

Reader :: struct {
	s: string, // read-only buffer
	i: i64, // current reading index
}

reader_init :: proc(r: ^Reader, s: string) {
	r.s = s
	r.i = 0
}

reader_next_rune :: proc(r: ^Reader) -> (rr: rune, size: int, eof: bool) {
	if r.i >= i64(len(r.s)) {
		r.i += 1
		return 0, 0, true
	}
	if c := r.s[r.i]; c < utf8.RUNE_SELF {
		r.i += 1
		return rune(c), 1, false
	}
	rr, size = utf8.decode_rune_in_string(r.s[r.i:])
	r.i += i64(size)
	return
}

lexer_new :: proc(input: string) -> (lexer: Lexer) {
	reader: Reader
	reader_init(&reader, input)
	for c, size, eof := reader_next_rune(&reader); !eof; c, size, eof = reader_next_rune(&reader) {
		(!u.is_white_space(c)) or_continue
		switch c {
		case '_', 'a' ..= 'z', 'A' ..= 'Z', 'æ' ..= 'ø':
			start := reader.i - i64(size)
			for !eof && u.is_alpha(c) || u.is_number(c) || c == '_' {
				c, size, eof = reader_next_rune(&reader)
			}
			reader.i -= i64(!eof ? size : 1)
			token := Token {
				kind = TokenKind.Ident,
				text = reader.s[start:reader.i],
			}
			append(&lexer.tokens, token)
		case '0' ..= '9':
            is_float := false
			start := reader.i - i64(size)
			for !eof && u.is_number(c) {
				c, size, eof = reader_next_rune(&reader)
			}
            if (c == '.') {
                is_float = true
                c, size, eof = reader_next_rune(&reader)
                for !eof && u.is_number(c) {
                    c, size, eof = reader_next_rune(&reader)
                }
            }
			reader.i -= i64(!eof ? size : 1)
			token := Token {
				kind = is_float ? TokenKind.Float : TokenKind.Integer,
				text = reader.s[start:reader.i],
			}
			append(&lexer.tokens, token)
		case '*':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Mul, text = reader.s[reader.i - 1:reader.i]},
			)
		case '-':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Sub, text = reader.s[reader.i - 1:reader.i]},
			)
		case '+':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Add, text = reader.s[reader.i - 1:reader.i]},
			)
		case '/':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Div, text = reader.s[reader.i - 1:reader.i]},
			)
		case '^':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Exp, text = reader.s[reader.i - 1:reader.i]},
			)
		case '(':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.LPar, text = reader.s[reader.i - 1:reader.i]},
			)
		case ')':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.RPar, text = reader.s[reader.i - 1:reader.i]},
			)
		case '[':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.LSquareB, text = reader.s[reader.i - 1:reader.i]},
			)
		case ']':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.RSquareB, text = reader.s[reader.i - 1:reader.i]},
			)
		case '!':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Factorial, text = reader.s[reader.i - 1:reader.i]},
			)
		case '?':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.QuestionMark, text = reader.s[reader.i - 1:reader.i]},
			)
		case ':':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Colon, text = reader.s[reader.i - 1:reader.i]},
			)
		case '=':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Eq, text = reader.s[reader.i - 1:reader.i]},
			)
		case '.':
			append(
				&lexer.tokens,
				Token{kind = TokenKind.Compose, text = reader.s[reader.i - 1:reader.i]},
			)
		}
	}
	append(&lexer.tokens, Token{kind = TokenKind.EOF})
	return
}

lexer_advance :: #force_inline proc(lexer: ^Lexer) -> (token: Token) {
    token = lexer.tokens[lexer.current]
    lexer.current += 1
	return
}

lexer_current :: #force_inline proc(lexer: ^Lexer) -> Token {
	return lexer.tokens[lexer.current]
}

lexer_reset :: #force_inline proc(lexer: ^Lexer) {
    lexer.current = 0
}

infix_binding_power :: proc(token: Token) -> (lbp: int, rbp: int, ok: bool) {
	#partial switch token.kind {
	case .Add, .Sub:
		return 5, 6, true
	case .Mul, .Div:
		return 7, 8, true
	case .Exp:
		return 5, 6, true
	case .QuestionMark:
		return 4, 3, true
	case .Eq:
		return 2, 1, true
	case .Compose:
		return 14, 13, true
	}
	return -1, -1, false
}

prefix_binding_power :: proc(token: Token) -> (bp: int, ok: bool) {
	#partial switch token.kind {
	case .Add, .Sub:
		return 9, true
	}
	return -1, false
}

postfix_binding_power :: proc(token: Token) -> (bp: int, ok: bool) {
	#partial switch token.kind {
	case .Factorial, .LPar:
		return 11, true
	}
	return -1, false
}

lexer_parse :: proc(lexer: ^Lexer) -> (exp: ^Expr) {
	return expr(lexer, 0)
}

expr_new :: #force_inline proc(e: Expr) -> (ret: ^Expr) {
	ret = new(Expr)
	ret^ = e
	return
}

expr :: proc(lexer: ^Lexer, limit: int) -> (lhs: ^Expr) {
	lbp, rbp: int;ok: bool
	lhs = new(Expr)
	first_token := lexer_advance(lexer)
	#partial switch first_token.kind {
	case ._AtomBegin ..= ._AtomEnd:
		#partial switch first_token.kind {
		case .Integer:
			lhs^ = Expr {
				operands = {integer = Integer{strconv.atoi(first_token.text)}},
				op = .Integer,
			}
		case .Float:
			lhs^ = Expr {
				operands = {float = Float{f32(strconv.atof(first_token.text))}},
				op = .Float,
			}
		case .Ident:
			lhs^ = Expr {
				operands = {ident = Ident{first_token.text}},
				op = .Ident,
			}
		}
	case ._OpBegin ..= ._OpEnd:
		if first_token.kind == .LPar {
			lhs = expr(lexer, 0)
			assert(lexer_advance(lexer).kind == .RPar)
		} else {
			power, ok := prefix_binding_power(first_token)
			if ok {
				rhs := expr(lexer, power)
				lhs^ = Expr {
					operands = {unary_expr = UnaryExpr{e = rhs}},
					op = first_token.kind,
				}
			} else {
				panic("Bad token")
			}
		}
	case:
		panic("Bad token")
	}

	for {
		second_token := lexer_current(lexer)
		if second_token.kind == .EOF {break}
		#partial switch second_token.kind {
		case ._OpBegin ..= ._OpEnd:
		case:
			panic("Bad token")
		}

		lbp, ok = postfix_binding_power(second_token)
		if ok {
			if lbp < limit {break}
			lexer_advance(lexer)
			if second_token.kind == .LSquareB {
				rhs := expr(lexer, 0)
				assert(lexer_advance(lexer).kind == .RSquareB)
				lhs = expr_new(
					Expr {
						operands = {binary_expr = BinaryExpr{l = lhs, r = rhs}},
						op = second_token.kind,
					},
				)
			} else {
				lhs = expr_new(
					Expr{operands = {unary_expr = UnaryExpr{e = lhs}}, op = second_token.kind},
				)
			}
			continue
		}

		lbp, rbp, ok := infix_binding_power(second_token)
		if ok {
			if lbp < limit {break}
			lexer_advance(lexer)
			if second_token.kind == .QuestionMark {
				mhs := expr(lexer, 0)
				third_token := lexer_advance(lexer)
				assert(third_token.kind == .Colon)
				rhs := expr(lexer, rbp)
				lhs = expr_new(
					Expr {
						operands = {ternary_expr = TernaryExpr{l = lhs, r = rhs, m = mhs}},
						op = second_token.kind,
					},
				)
			} else {
				rhs := expr(lexer, rbp)
				lhs = expr_new(
					Expr {
						operands = {binary_expr = BinaryExpr{l = lhs, r = rhs}},
						op = second_token.kind,
					},
				)
			}
			continue
		}

		break
	}

	return
}

expr_print :: proc(expression: ^Expr) {
	#partial switch expression.op {
	case .Integer:
		fmt.print(expression.operands.integer.value)
    case .Float:
        fmt.print(expression.operands.float.value)
	case .Ident:
		fmt.print(expression.operands.ident.name)
	case ._UnaryBegin ..= ._UnaryEnd:
		fmt.printf("{0}", expression.op)
		expr_print(expression.operands.unary_expr.e)
	case ._BinaryBegin ..= ._BinaryEnd:
		exp := expression.operands.binary_expr
		fmt.print("(")
		fmt.printf("{0} ", expression.op)
		expr_print(exp.l);fmt.print(" ");expr_print(exp.r)
		fmt.print(")")
	case ._TernaryBegin ..= ._TernaryEnd:
		exp := expression.operands.ternary_expr
		fmt.print("(")
		fmt.printf("{0} ", expression.op)
		expr_print(exp.l);fmt.print(" ");expr_print(exp.m);fmt.print(" ");expr_print(exp.r)
		fmt.print(")")
	}
}

main :: proc() {
	temp_track: mem.Tracking_Allocator
	mem.tracking_allocator_init(&temp_track, context.temp_allocator)
	context.temp_allocator = mem.tracking_allocator(&temp_track)

	defer {
		if len(temp_track.allocation_map) > 0 {
			fmt.eprintf("=== %v allocations not freed: ===\n", len(temp_track.allocation_map))
			for _, entry in temp_track.allocation_map {
				fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
			}
		}
		if len(temp_track.bad_free_array) > 0 {
			fmt.eprintf("=== %v incorrect frees: ===\n", len(temp_track.bad_free_array))
			for entry in temp_track.bad_free_array {
				fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
			}
		}
		mem.tracking_allocator_destroy(&temp_track)
	}

	track: mem.Tracking_Allocator
	mem.tracking_allocator_init(&track, context.allocator)
	context.allocator = mem.tracking_allocator(&track)

	defer {
		if len(track.allocation_map) > 0 {
			fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
			for _, entry in track.allocation_map {
				fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
			}
		}
		if len(track.bad_free_array) > 0 {
			fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
			for entry in track.bad_free_array {
				fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
			}
		}
		mem.tracking_allocator_destroy(&track)
	}

	{
		context.allocator = context.temp_allocator
		defer free_all(context.temp_allocator)

		lexer := lexer_new("1.2 * 3.5124 + 123 - 100 * (f . g)")
		e := lexer_parse(&lexer)
		expr_print(e)
	}
}
