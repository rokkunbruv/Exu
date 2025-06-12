package interpreter

import (
	"testing"

	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expr"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
	"github.com/stretchr/testify/assert"
)

func TestEvaluate(t *testing.T) {
	type testCase struct {
		name        string
		interpreter Interpreter

		expected literal.Literal
		err      error
	}

	tests := []testCase{
		{
			name: "unary not on bool",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(true)},
				},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "double unary not on bool",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
					Right: &expr.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
						Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(true)},
					},
				},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "unary not on expression that evaluates to bool",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
					Right: &expr.Binary{
						Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
						Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
					},
				},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "unary not on string",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
					Right:    &expr.Literal{Value: literal.GenerateStringLiteral("str")},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!"},
				Message: "Operation ! cannot be performed on type String",
			},
		},
		{
			name: "unary not on numeric",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(0)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!"},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "unary not on null",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
					Right:    &expr.Literal{Value: &literal.NullLiteral{}},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!"},
				Message: "Operation ! cannot be performed on type Null",
			},
		},
		{
			name: "unary minus on numeric",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(23)},
				},
			},
			expected: literal.GenerateNumericLiteral(-23),
		},
		{
			name: "double unary minus on numeric",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
					Right: &expr.Unary{
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
						Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(23)},
					},
				},
			},
			expected: literal.GenerateNumericLiteral(23),
		},
		{
			name: "unary minus on expression that evaluates to numeric",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
					Right: &expr.Binary{
						Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
						Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
					},
				},
			},
			expected: literal.GenerateNumericLiteral(-3),
		},
		{
			name: "unary minus on string",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateStringLiteral("str")},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type String",
			},
		},
		{
			name: "unary minus on bool",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(true)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type Bool",
			},
		},
		{
			name: "unary minus on null",
			interpreter: Interpreter{
				Expression: &expr.Unary{
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
					Right:    &expr.Literal{Value: &literal.NullLiteral{}},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type Null",
			},
		},

		{
			name: "equal on two expressions that lead to same type",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left: &expr.Binary{
						Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
						Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
					},
					Operator: token.Token{TokenType: token.EQUAL, Lexeme: "="},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "equal on two expressions that doesnt lead to same type",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left: &expr.Binary{
						Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
						Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
					},
					Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.EQUAL, Lexeme: "=", Line: 0},
				Message: "Operation = cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "not equal on two expressions that lead to same type",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left: &expr.Binary{
						Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
						Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
					},
					Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!="},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "not equal on two expressions that doesnt lead to same type",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left: &expr.Binary{
						Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
						Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
					},
					Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Line: 0},
				Message: "Operation != cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "greater on two numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "greater on one non numeric",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 0},
				Message: "Operation > cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "greater on two non numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 0},
				Message: "Operation > cannot be performed on type String and type Bool",
			},
		},
		{
			name: "greater equal on two numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">="},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "greater equal on one non numeric",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Line: 0},
				Message: "Operation >= cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "greater equal on two non numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Line: 0},
				Message: "Operation >= cannot be performed on type String and type Bool",
			},
		},
		{
			name: "less on two numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<"},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "less on one non numeric",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Line: 0},
				Message: "Operation < cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "less on two non numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Line: 0},
				Message: "Operation < cannot be performed on type String and type Bool",
			},
		},
		{
			name: "less equal on two numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<="},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "less equal on one non numeric",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Line: 0},
				Message: "Operation <= cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "less equal on two non numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Line: 0},
				Message: "Operation <= cannot be performed on type String and type Bool",
			},
		},
		{
			name: "minus on two numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: literal.GenerateNumericLiteral(-1),
		},
		{
			name: "minus on one non numeric",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: &literal.NullLiteral{}},
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "minus on two non numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(true)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type String and type Bool",
			},
		},
		{
			name: "plus on two numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: literal.GenerateNumericLiteral(3),
		},
		{
			name: "plus on two strings",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
					Right:    &expr.Literal{Value: literal.GenerateStringLiteral("str")},
				},
			},
			expected: literal.GenerateStringLiteral("strstr"),
		},
		{
			name: "plus on one non numeric/string",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: &literal.NullLiteral{}},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
				Message: "Operation + cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "plus on two non numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateBoolLiteral(false)},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(true)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
				Message: "Operation + cannot be performed on type Bool and type Bool",
			},
		},
		{
			name: "plus on numeric and string",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
				Message: "Operation + cannot be performed on type String and type Numeric",
			},
		},
		{
			name: "star on two numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.STAR, Lexeme: "*"},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: literal.GenerateNumericLiteral(2),
		},
		{
			name: "star on one non numeric",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: &literal.NullLiteral{}},
					Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.STAR, Lexeme: "*", Line: 0},
				Message: "Operation * cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "star on two non numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(true)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.STAR, Lexeme: "*", Line: 0},
				Message: "Operation * cannot be performed on type String and type Bool",
			},
		},
		{
			name: "slash on two numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.SLASH, Lexeme: "/"},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: literal.GenerateNumericLiteral(0.5),
		},
		{
			name: "slash on one non numeric",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: &literal.NullLiteral{}},
					Operator: token.Token{TokenType: token.SLASH, Lexeme: "/", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.SLASH, Lexeme: "/", Line: 0},
				Message: "Operation / cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "slash on two non numerics",
			interpreter: Interpreter{
				Expression: &expr.Binary{
					Left:     &expr.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.SLASH, Lexeme: "/", Line: 0},
					Right:    &expr.Literal{Value: literal.GenerateBoolLiteral(true)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.SLASH, Lexeme: "/", Line: 0},
				Message: "Operation / cannot be performed on type String and type Bool",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.interpreter.evaluate(test.interpreter.Expression)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}
