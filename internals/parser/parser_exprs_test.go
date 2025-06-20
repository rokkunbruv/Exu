package parser

import (
	"testing"

	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expression"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
	"github.com/stretchr/testify/assert"
)

func TestCall(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test call on variable",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Call{
				Callee: &expression.Variable{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				},
				Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Arguments: []expression.Expr{},
			},
			err: nil,
		},
		{
			name: "test call on primary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Call{
				Callee:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Arguments: []expression.Expr{},
			},
			err: nil,
		},
		{
			name: "test call w/ one var arg",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "n", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Call{
				Callee: &expression.Variable{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				},
				Paren: token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Arguments: []expression.Expr{
					&expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "n", Line: 1}},
				},
			},
			err: nil,
		},
		{
			name: "test call w/ multiple var args",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "n1", Line: 1},
				{TokenType: token.COMMA, Lexeme: ",", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "n2", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Call{
				Callee: &expression.Variable{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				},
				Paren: token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Arguments: []expression.Expr{
					&expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "n1", Line: 1}},
					&expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "n2", Line: 1}},
				},
			},
			err: nil,
		},
		{
			name: "test call w/ expression arg",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Call{
				Callee: &expression.Variable{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				},
				Paren: token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Arguments: []expression.Expr{
					&expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					},
				},
			},
			err: nil,
		},
		{
			name: "test call w/o left paren",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "n", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Variable{
				Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
			},
			err: nil,
		},
		{
			name: "test call w/o right paren",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "n", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected \")\" after arguments",
			},
		},
		{
			name: "test call w/ trailing comma",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "n", Line: 1},
				{TokenType: token.COMMA, Lexeme: ",", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test call w/ invalid arg",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test call w/ 256 arguments",
			parser: Parser{
				Tokens: func() []token.Token {
					tokens := []token.Token{
						{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
						{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
					}
					// Add 256 numeric arguments
					for i := range 256 {
						tokens = append(tokens, token.Token{
							TokenType: token.NUMERIC,
							Lexeme:    "1",
							Literal:   literal.GenerateNumericLiteral(1),
							Line:      1,
						})
						if i < 255 {
							tokens = append(tokens, token.Token{
								TokenType: token.COMMA,
								Lexeme:    ",",
								Line:      1,
							})
						}
					}
					tokens = append(tokens,
						token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
						token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
					)
					return tokens
				}(),
				Curr: 0,
			},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.COMMA, Lexeme: ",", Line: 1},
				Message: "A call cannot have more than 255 arguments",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.and()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestOr(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid or expression between literals",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.OR, Lexeme: "|", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Logical{
				Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				Operator: token.Token{TokenType: token.OR, Lexeme: "|", Line: 1},
				Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
			},
			err: nil,
		},
		{
			name: "test chained or expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.OR, Lexeme: "|", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.OR, Lexeme: "|", Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: literal.GenerateBoolLiteral(false), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Logical{
				Left: &expression.Logical{
					Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					Operator: token.Token{TokenType: token.OR, Lexeme: "|", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				},
				Operator: token.Token{TokenType: token.OR, Lexeme: "|", Line: 1},
				Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
			},
			err: nil,
		},
		{
			name: "test valid or expression between expressions",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: literal.GenerateNumericLiteral(2), Line: 1},
				{TokenType: token.OR, Lexeme: "|", Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: literal.GenerateStringLiteral("str"), Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: literal.GenerateStringLiteral("str"), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Logical{
				Left: &expression.Binary{
					Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
				Operator: token.Token{TokenType: token.OR, Lexeme: "|", Line: 1},
				Right: &expression.Binary{
					Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateStringLiteral("str")},
				},
			},
			err: nil,
		},
		{
			name: "test no right in or expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.OR, Lexeme: "|", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test no left in or expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.OR, Lexeme: "|", Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: literal.GenerateBoolLiteral(false), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.OR, Lexeme: "|", Line: 1},
				Message: "Expected expression but got |",
			},
		},
		{
			name: "test invalid expression in or expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Line: 1},
				{TokenType: token.OR, Lexeme: "|", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.OR, Lexeme: "|", Line: 1},
				Message: "Expected expression but got |",
			},
		},
		{
			name: "test valid and expr",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Logical{
				Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				Operator: token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
				Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
			},
			err: nil,
		},
		{
			name: "test invalid and expr",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected expression but got ",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.or()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestAnd(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid and expression between literals",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Logical{
				Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				Operator: token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
				Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
			},
			err: nil,
		},
		{
			name: "test chained and expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: literal.GenerateBoolLiteral(false), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Logical{
				Left: &expression.Logical{
					Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					Operator: token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				},
				Operator: token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
				Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
			},
			err: nil,
		},
		{
			name: "test valid and expression between expressions",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: literal.GenerateNumericLiteral(2), Line: 1},
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: literal.GenerateStringLiteral("str"), Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: literal.GenerateStringLiteral("str"), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Logical{
				Left: &expression.Binary{
					Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
				Operator: token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
				Right: &expression.Binary{
					Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateStringLiteral("str")},
				},
			},
			err: nil,
		},
		{
			name: "test no right in and expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test no left in and expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: literal.GenerateBoolLiteral(false), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
				Message: "Expected expression but got &",
			},
		},
		{
			name: "test invalid expression in and expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Line: 1},
				{TokenType: token.AND, Lexeme: "&", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
				Message: "Expected expression but got &",
			},
		},
		{
			name: "test valid equality",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Binary{
				Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Line: 1},
				Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
			},
			err: nil,
		},
		{
			name: "test invalid equality",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.EQUAL, Lexeme: "=", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected expression but got ",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.and()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestExpression(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test primary expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "42.5", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(42.5)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Literal{Value: func() *literal.NumericLiteral {
				lit := &literal.NumericLiteral{}
				lit.SetVal(42.5)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test unary expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test factor expression",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}()},
				Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test term expression",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test comparison expression",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(5)
					return lit
				}()},
				Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(10)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test equality expression",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test nested unary expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expression.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
					Right: &expression.Literal{Value: func() *literal.BoolLiteral {
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
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Grouping{
					Expression: &expression.Binary{
						Left: &expression.Literal{Value: func() *literal.NumericLiteral {
							lit := &literal.NumericLiteral{}
							lit.SetVal(1)
							return lit
						}()},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
						Right: &expression.Literal{Value: func() *literal.NumericLiteral {
							lit := &literal.NumericLiteral{}
							lit.SetVal(2)
							return lit
						}()},
					},
				},
				Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test complex nested expression",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
					Right: &expression.Grouping{
						Expression: &expression.Binary{
							Left: &expression.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(1)
								return lit
							}()},
							Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
							Right: &expression.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(2)
								return lit
							}()},
						},
					},
				},
				Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Right: &expression.Binary{
					Left: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(3)
						return lit
					}()},
					Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
					Right: &expression.Grouping{
						Expression: &expression.Binary{
							Left: &expression.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(4)
								return lit
							}()},
							Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
							Right: &expression.Literal{Value: func() *literal.NumericLiteral {
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
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Binary{
					Left: &expression.Binary{
						Left: &expression.Literal{Value: func() *literal.NumericLiteral {
							lit := &literal.NumericLiteral{}
							lit.SetVal(1)
							return lit
						}()},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
						Right: &expression.Binary{
							Left: &expression.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(2)
								return lit
							}()},
							Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
							Right: &expression.Literal{Value: func() *literal.NumericLiteral {
								lit := &literal.NumericLiteral{}
								lit.SetVal(3)
								return lit
							}()},
						},
					},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
					Right: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(4)
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(5)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name:     "test empty token list",
			parser:   Parser{Tokens: []token.Token{}, Curr: 0},
			expected: nil,
			err:      &exu_err.ParseError{Curr: 0, Message: "Index out of bounds"},
		},
		{
			name: "test missing closing parenthesis",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected ')' after expression",
			},
		},
		// FIXME: Go back to this
		// {
		// 	name: "test extra closing parenthesis",
		// 	parser: Parser{Tokens: []token.Token{
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
		// 	}, Curr: 0},
		// 	expected: nil,
		// 	err:      fmt.Errorf("expect expression at )"),
		// },
		{
			name: "test invalid token in expression",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Literal{Value: func() *literal.NumericLiteral {
				lit := &literal.NumericLiteral{}
				lit.SetVal(1)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test invalid expression after valid one",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(2)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test maximum operator nesting depth",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Grouping{
				Expression: &expression.Grouping{
					Expression: &expression.Grouping{
						Expression: &expression.Grouping{
							Expression: &expression.Literal{Value: func() *literal.NumericLiteral {
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
		{
			name: "test valid assignment",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Assignment{
				Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Value: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid l-value in assignment",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.COLON, Lexeme: ":", Line: 1},
				Message: "Invalid assignment target",
			},
		},
		{
			name: "test incomplete assignment",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
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

func TestEquality(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid equality with equal",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid equality with not equal",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: &literal.NullLiteral{}, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				Right:    &expression.Literal{Value: &literal.NullLiteral{}},
			},
			err: nil,
		},
		{
			name: "test incomplete equality with no first comparison",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Message: "Expected expression but got =",
			},
		},
		{
			name: "test incomplete equality with no second comparison",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test invalid equality with invalid first comparison",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
				Message: "Expected expression but got =",
			},
		},
		{
			name: "test invalid equality with invalid second comparison",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test nested equality",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Binary{
					Left: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 1},
					Right: &expression.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("str")
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid comparison",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid comparison",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
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

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid comparison with greater than",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid comparison with greater equal",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid comparison with less than",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: &literal.NullLiteral{}, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Right:    &expression.Literal{Value: &literal.NullLiteral{}},
			},
			err: nil,
		},
		{
			name: "test valid comparison with less equal",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: &literal.NullLiteral{}, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: nil, Line: 1},
				Right:    &expression.Literal{Value: &literal.NullLiteral{}},
			},
			err: nil,
		},
		{
			name: "test incomplete comparison with no first term",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				Message: "Expected expression but got >",
			},
		},
		{
			name: "test incomplete comparison with no second term",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test invalid comparison with invalid first term",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				Message: "Expected expression but got >",
			},
		},
		{
			name: "test invalid comparison with invalid second term",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test nested comparisons",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Binary{
					Left: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
					Right: &expression.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("str")
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid term",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid term",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
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

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid term with plus",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid term with minus",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: &literal.NullLiteral{}, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right:    &expression.Literal{Value: &literal.NullLiteral{}},
			},
			err: nil,
		},
		{
			name: "test incomplete term with no first factor",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Message: "Expected expression but got +",
			},
		},
		{
			name: "test incomplete term with no second factor",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test unary with minus",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: &literal.NullLiteral{}, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Unary{
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right:    &expression.Literal{Value: &literal.NullLiteral{}},
			},
			err: nil,
		},
		{
			name: "test invalid term with invalid first factor",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				Message: "Expected expression but got /",
			},
		},
		{
			name: "test invalid term with invalid second factor",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test nested terms",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Binary{
					Left: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
					Right: &expression.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("str")
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid factor",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
				Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid factor",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
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

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test valid factor with slash",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(3)
					return lit
				}()},
				Operator: token.Token{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(4)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid factor with star",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Literal{Value: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}()},
				Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid factor with no first unary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.NULL, Lexeme: "null", Literal: &literal.NullLiteral{}, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				Message: "Expected expression but got *",
			},
		},
		{
			name: "test invalid factor with no second unary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test invalid factor with invalid first unary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"true\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("true")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 1},
				Message: "Expected expression but got >",
			},
		},
		{
			name: "test invalid factor with invalid second unary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
				{TokenType: token.DOT, Lexeme: ".", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.DOT, Lexeme: ".", Literal: nil, Line: 1},
				Message: "Expected expression but got .",
			},
		},
		{
			name: "test nested factor",
			parser: Parser{Tokens: []token.Token{
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
			}, Curr: 0},
			expected: &expression.Binary{
				Left: &expression.Binary{
					Left: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 1},
					Right: &expression.Literal{Value: func() *literal.StringLiteral {
						lit := &literal.StringLiteral{}
						lit.SetVal("true")
						return lit
					}()},
				},
				Operator: token.Token{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid unary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test invalid unary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
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

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test not operator with unary operand",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expression.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
					Right: &expression.Literal{Value: func() *literal.BoolLiteral {
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
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Unary{
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right: &expression.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
					Right: &expression.Literal{Value: func() *literal.StringLiteral {
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
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Unary{
				Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test minus operator with primary operand",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "45", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(45)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Unary{
				Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				Right: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(45)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test not operator with invalid unary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NOT, Lexeme: "!", Literal: nil, Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Message: "Expected expression but got +",
			},
		},
		{
			name: "test minus operator with invalid unary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Message: "Expected expression but got <",
			},
		},
		{
			name: "test minus operator with invalid primary",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 1},
				Message: "Expected expression but got +",
			},
		},
		{
			name: "test valid primary expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Literal{Value: func() *literal.NumericLiteral {
				lit := &literal.NumericLiteral{}
				lit.SetVal(12.34)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test invalid primary expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
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

		expected expression.Expr
		err      error
	}

	tests := []testCase{
		{
			name: "test false literal",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Literal{Value: func() *literal.BoolLiteral {
				lit := &literal.BoolLiteral{}
				lit.SetVal(false)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test true literal",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(true)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Literal{Value: func() *literal.BoolLiteral {
				lit := &literal.BoolLiteral{}
				lit.SetVal(true)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test null literal",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NULL, Lexeme: "null", Literal: &literal.NullLiteral{}, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Literal{Value: &literal.NullLiteral{}},
			err:      nil,
		},
		{
			name: "test numeric literal",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "12.34", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(12.34)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Literal{Value: func() *literal.NumericLiteral {
				lit := &literal.NumericLiteral{}
				lit.SetVal(12.34)
				return lit
			}()},
			err: nil,
		},
		{
			name: "test string literal",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"str\"", Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("str")
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Literal{Value: func() *literal.StringLiteral {
				lit := &literal.StringLiteral{}
				lit.SetVal("str")
				return lit
			}()},
			err: nil,
		},
		{
			name: "test grouping with matching parentheses",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: &expression.Grouping{Expression: &expression.Literal{Value: func() *literal.BoolLiteral {
				lit := &literal.BoolLiteral{}
				lit.SetVal(false)
				return lit
			}()}},
			err: nil,
		},
		{
			name: "test grouping with missing right parenthesis",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: func() *literal.BoolLiteral {
					lit := &literal.BoolLiteral{}
					lit.SetVal(false)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
				Message: "Expected ')' after expression",
			},
		},
		{
			name: "test grouping with invalid expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: nil, Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: nil, Line: 1},
				{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 1},
				Message: "Expected expression but got <",
			},
		},
		{
			name: "test valid variable",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &expression.Variable{
				Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
			},
			err: nil,
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
