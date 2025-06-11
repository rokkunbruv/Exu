package parser

import (
	"testing"

	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expr"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
	"github.com/stretchr/testify/assert"
)

func TestExpression(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expr.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test primary expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "42.5", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(42.5)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Literal{Value: func() *literal.NumericLiteral {
				lit := &literal.NumericLiteral{}
				lit.SetVal(42.5)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test unary expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test factor expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}()},
				Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test term expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test comparison expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "5", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(5)
					return lit
				}(), Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "10", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(10)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(5)
					return lit
				}()},
				Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(10)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test equality expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test nested unary expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
					Right: &expr.Literal{Value: func() *literal.BoolLiteral {
						lit := &literal.BoolLiteral{}
						lit.SetVal(true)
						return lit
					}()},
				},
			},
			err: nil,
		},
		{
			name: "test nested arithmetic expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Grouping{
					Expression: &expr.Binary{
						Left: &expr.Literal{Value: func() *literal.NumericLiteral {
							lit := &literal.NumericLiteral{}
							lit.SetVal(1)
							return lit
						}()},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
						Right: &expr.Literal{Value: func() *literal.NumericLiteral {
							lit := &literal.NumericLiteral{}
							lit.SetVal(2)
							return lit
						}()},
					},
				},
				Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test complex nested expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "4", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(4)
					return lit
				}(), Line: 1},
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "5", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(5)
					return lit
				}(), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
					Right: &expr.Grouping{
						Expression: &expr.Binary{
							Left: &expr.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(1)
								return lit
							}()},
							Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
							Right: &expr.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(2)
								return lit
							}()},
						},
					},
				},
				Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Right: &expr.Binary{
					Left: &expr.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(3)
						return lit
					}()},
					Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
					Right: &expr.Grouping{
						Expression: &expr.Binary{
							Left: &expr.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(4)
								return lit
							}()},
							Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
							Right: &expr.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(5)
								return lit
							}()},
						},
					},
				},
			},
			err: nil,
		},

		{
			name: "test expression with all operator types",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}(), Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "4", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(4)
					return lit
				}(), Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "5", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(5)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Binary{
					Left: &expr.Binary{
						Left: &expr.Literal{Value: func() *literal.NumericLiteral {
							lit := &literal.NumericLiteral{}
							lit.SetVal(1)
							return lit
						}()},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
						Right: &expr.Binary{
							Left: &expr.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(2)
								return lit
							}()},
							Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
							Right: &expr.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(3)
								return lit
							}()},
						},
					},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
					Right: &expr.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(4)
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(5)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name:     "test empty token list",
			parser:   Parser{tokens: []token.Token{}, curr: 0},
			expected: nil,
			err:      &exu_err.ParseError{Curr: 0, Message: "Index out of bounds"},
		},
		{
			name: "test missing closing parenthesis",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected ')' after expression",
			},
		},
		// FIXME: Go back to this
		// {
		// 	name: "test extra closing parenthesis",
		// 	parser: Parser{tokens: []token.Token{
		// 		{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
		// 			lit := &literal.NumericLiteral{}
		// 			lit.SetVal(1)
		// 			return lit
		// 		}(), Line: 1},
		// 		{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
		// 		{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
		// 			lit := &literal.NumericLiteral{}
		// 			lit.SetVal(2)
		// 			return lit
		// 		}(), Line: 1},
		// 		{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
		// 		{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
		// 	}, curr: 0},
		// 	expected: nil,
		// 	err:      fmt.Errorf("expect expression at )"),
		// },
		{
			name: "test invalid token in expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "at", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Literal{Value: func() *literal.NumericLiteral {
				lit := &literal.NumericLiteral{}
				lit.SetVal(1)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test invalid expression after valid one",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "_", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test maximum operator nesting depth",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Grouping{
				Expression: &expr.Grouping{
					Expression: &expr.Grouping{
						Expression: &expr.Grouping{
							Expression: &expr.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(1)
								return lit
							}()},
						},
					},
				},
			},
			err: nil,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.expression()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestEquality(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expr.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid equality with equal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid equality with not equal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				Right:    &expr.Literal{Value: nil},
			},
			err: nil,
		},
		{
			name: "test incomplete equality with no first comparison",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Message: "Expected expression but got =",
			},
		},
		{
			name: "test incomplete equality with no second comparison",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test invalid equality with invalid first comparison",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Message: "Expected expression but got =",
			},
		},
		{
			name: "test invalid equality with invalid second comparison",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.3)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test nested equality",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Binary{
					Left: &expr.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
					Right: &expr.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("str")
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid comparison",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid comparison",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.expression()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestComparison(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expr.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid comparison with greater than",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid comparison with greater equal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid comparison with less than",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Right:    &expr.Literal{Value: nil},
			},
			err: nil,
		},
		{
			name: "test valid comparison with less equal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: nil, Line: 1},
				Right:    &expr.Literal{Value: nil},
			},
			err: nil,
		},
		{
			name: "test incomplete comparison with no first term",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				Message: "Expected expression but got >",
			},
		},
		{
			name: "test incomplete comparison with no second term",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test invalid comparison with invalid first term",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				Message: "Expected expression but got >",
			},
		},
		{
			name: "test invalid comparison with invalid second term",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.3)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test nested comparisons",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Binary{
					Left: &expr.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
					Right: &expr.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("str")
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid term",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid term",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Message: "Expected expression but got +",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.expression()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestTerm(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expr.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid term with plus",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid term with minus",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right:    &expr.Literal{Value: nil},
			},
			err: nil,
		},
		{
			name: "test incomplete term with no first factor",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Message: "Expected expression but got +",
			},
		},
		{
			name: "test incomplete term with no second factor",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test unary with minus",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Unary{
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right:    &expr.Literal{Value: nil},
			},
			err: nil,
		},
		{
			name: "test invalid term with invalid first factor",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				Message: "Expected expression but got /",
			},
		},
		{
			name: "test invalid term with invalid second factor",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.3)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test nested terms",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Binary{
					Left: &expr.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
					Right: &expr.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("str")
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid factor",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid factor",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Message: "Expected expression but got *",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.expression()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestFactor(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expr.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid factor with slash",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "3", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}(), Line: 1},
				{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "4", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(4)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
				Operator: token.Token{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(4)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid factor with star",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
				Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid factor with no first unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Message: "Expected expression but got *",
			},
		},
		{
			name: "test invalid factor with no second unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test invalid factor with invalid first unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"true\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("true")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				Message: "Expected expression but got >",
			},
		},
		{
			name: "test invalid factor with invalid second unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.DOT, Lexeme: ".", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.DOT, Lexeme: ".", Literal: nil, Line: 1},
				Message: "Expected expression but got .",
			},
		},
		{
			name: "test nested factor",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"true\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("true")
					return lit
				}(), Line: 1},
				{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Binary{
				Left: &expr.Binary{
					Left: &expr.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
					Right: &expr.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("true")
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.expression()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestUnary(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expr.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test not operator with unary operand",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
					Right: &expr.Literal{Value: func() *literal.BoolLiteral {
						lit := &literal.BoolLiteral{}
						lit.SetVal(true)
						return lit
					}()},
				},
			},
			err: nil,
		},
		{
			name: "test minus operator with not unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Unary{
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right: &expr.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
					Right: &expr.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("str")
						return lit
					}()},
				},
			},
			err: nil,
		},
		{
			name: "test not operator with primary operand",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test minus operator with primary operand",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "45", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(45)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Unary{
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right: &expr.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(45)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test not operator with invalid unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Message: "Expected expression but got +",
			},
		},
		{
			name: "test minus operator with invalid unary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Message: "Expected expression but got <",
			},
		},
		{
			name: "test not operator with invalid primary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "tru", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "tru", Literal: nil, Line: 1},
				Message: "Expected expression but got tru",
			},
		},
		{
			name: "test minus operator with invalid primary",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Message: "Expected expression but got +",
			},
		},
		{
			name: "test valid primary expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Literal{Value: func() *literal.NumericLiteral {
				lit := &literal.NumericLiteral{}
				lit.SetVal(12.34)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test invalid primary expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: nil, Line: 1},
				Message: "Expected expression but got <-",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.expression()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestPrimary(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expr.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test false literal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Literal{Value: func() *literal.BoolLiteral {
				lit := &literal.BoolLiteral{}
				lit.SetVal(false)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test true literal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Literal{Value: func() *literal.BoolLiteral {
				lit := &literal.BoolLiteral{}
				lit.SetVal(true)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test null literal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NULL, Lexeme: "null", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Literal{Value: nil},
			err:      nil,
		},
		{
			name: "test numeric literal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Literal{Value: func() *literal.NumericLiteral {
				lit := &literal.NumericLiteral{}
				lit.SetVal(12.34)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test string literal",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Literal{Value: func() *literal.StringLiteral {
				lit := &literal.StringLiteral{}
				lit.SetVal("str")
				return lit
			}()},
			err: nil,
		},
		{
			name: "test grouping with matching parentheses",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: &expr.Grouping{Expression: &expr.Literal{Value: func() *literal.BoolLiteral {
				lit := &literal.BoolLiteral{}
				lit.SetVal(false)
				return lit
			}()}},
			err: nil,
		},
		{
			name: "test grouping with missing right parenthesis",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected ')' after expression",
			},
		},
		{
			name: "test grouping with invalid expression",
			parser: Parser{tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Message: "Expected expression but got <",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.expression()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}
