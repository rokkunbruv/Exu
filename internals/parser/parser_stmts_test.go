package parser

import (
	"testing"

	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expression"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/statement"
	"github.com/rokkunbruv/internals/token"
	"github.com/stretchr/testify/assert"
)

func TestForStatement(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected statement.Stmt
		err      error
	}

	tests := []testCase{
		{
			name: "test for w/ var dec initializer, condition, & iterator",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.LET, Lexeme: "let", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Let{
						Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
						Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					},
					&statement.While{
						Condition: &expression.Binary{
							Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
							Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Line: 1},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						},
						Body: &statement.Block{
							Statements: []statement.Stmt{
								&statement.Expression{
									Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
								},
								&statement.Expression{
									Expression: &expression.Assignment{
										Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
										Value: &expression.Binary{
											Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
											Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
											Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
										},
									},
								},
							},
						},
					},
				},
			},
			err: nil,
		},
		{
			name: "test for w/ assignment initializer, condition, & iterator",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Assignment{
							Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
							Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						},
					},
					&statement.While{
						Condition: &expression.Binary{
							Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
							Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Line: 1},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						},
						Body: &statement.Block{
							Statements: []statement.Stmt{
								&statement.Expression{
									Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
								},
								&statement.Expression{
									Expression: &expression.Assignment{
										Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
										Value: &expression.Binary{
											Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
											Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
											Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
										},
									},
								},
							},
						},
					},
				},
			},
			err: nil,
		},
		{
			name: "test for w/o initializer, w/ condition & iterator",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.While{
				Condition: &expression.Binary{
					Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
				Body: &statement.Block{
					Statements: []statement.Stmt{
						&statement.Expression{
							Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
						},
						&statement.Expression{
							Expression: &expression.Assignment{
								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
								Value: &expression.Binary{
									Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
									Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
									Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								},
							},
						},
					},
				},
			},
			err: nil,
		},
		{
			name: "test for w/o initializer & condition, w/ iterator",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.While{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				Body: &statement.Block{
					Statements: []statement.Stmt{
						&statement.Expression{
							Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
						},
						&statement.Expression{
							Expression: &expression.Assignment{
								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
								Value: &expression.Binary{
									Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
									Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
									Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								},
							},
						},
					},
				},
			},
			err: nil,
		},
		{
			name: "test for w/o initializer, condition, & iterator",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.While{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				Body: &statement.Expression{
					Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
				},
			},
			err: nil,
		},
		{
			name: "test for w/o initializer & iterator, w/ condition",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.While{
				Condition: &expression.Binary{
					Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
					Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
				Body: &statement.Expression{
					Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
				},
			},
			err: nil,
		},
		{
			name: "test for w/o condition & iterator, w/ initializer",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Assignment{
							Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
							Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						},
					},
					&statement.While{
						Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
						Body: &statement.Expression{
							Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
						},
					},
				},
			},
			err: nil,
		},
		{
			name: "test for w/o condition, w/ initializer & iterator",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Assignment{
							Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
							Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						},
					},
					&statement.While{
						Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
						Body: &statement.Block{
							Statements: []statement.Stmt{
								&statement.Expression{
									Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
								},
								&statement.Expression{
									Expression: &expression.Assignment{
										Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
										Value: &expression.Binary{
											Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1}},
											Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
											Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
										},
									},
								},
							},
						},
					},
				},
			},
			err: nil,
		},
		{
			name: "test for w/ invalid initializer",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.LET, Lexeme: "let", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				Message: "Expected expression but got ;",
			},
		},
		{
			name: "test for w/ invalid condition",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.LET, Lexeme: "let", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Line: 1},
				Message: "Expected expression but got <",
			},
		},
		{
			name: "test for w/ invalid iterator",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				Message: "Expected expression but got +",
			},
		},
		{
			name: "test for w/ invalid body",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected \";\" after expression.",
			},
		},
		{
			name: "test for w/o left paren",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Message: "Expected \"(\" after \"for\"",
			},
		},
		{
			name: "test for w/o right paren",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Message: "Expected \")\" after for loop clause",
			},
		},
		{
			name: "test for w/o initializer & 1st semicolon",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				Message: "Expected \";\" after condition of for loop",
			},
		},
		{
			name: "test for w/o condition & 2nd semicolon",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected \";\" after condition of for loop",
			},
		},
		{
			name: "test for w/o condition & iterator & no semicolons",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected \";\" after expression.",
			},
		},
		{
			name: "test empty for clause",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test for w/o body",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test for w/ third semicolon",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.FOR, Lexeme: "for", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.LESS, Lexeme: "<", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				Message: "Expected \")\" after for loop clause",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.statement()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestWhileStatement(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected statement.Stmt
		err      error
	}

	tests := []testCase{
		{
			name: "test valid while statement",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.WHILE, Lexeme: "while", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.While{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				Body: &statement.Expression{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
				},
			},
			err: nil,
		},
		{
			name: "test missing left paren",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.WHILE, Lexeme: "while", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.TRUE, Lexeme: "true", Line: 1},
				Message: "Expected \"(\" after \"while\"",
			},
		},
		{
			name: "test missing right paren",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.WHILE, Lexeme: "while", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Message: "Expected \")\" after while condition",
			},
		},
		{
			name: "test valid while w/ expression condition",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.WHILE, Lexeme: "while", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: literal.GenerateNumericLiteral(2), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.While{
				Condition: &expression.Binary{
					Left: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
					Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
				Body: &statement.Expression{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
				},
			},
			err: nil,
		},
		{
			name: "test while w/ no body",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.WHILE, Lexeme: "while", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test while w/ invalid condition",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.WHILE, Lexeme: "while", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test while w/ invalid body",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.WHILE, Lexeme: "while", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.LEFT_BRACE, Lexeme: "{", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected \"}\" after block.",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.statement()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestIfStatement(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected statement.Stmt
		err      error
	}

	tests := []testCase{
		{
			name: "test if w/ then statement",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				ThenBranch: &statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
				},
			},
			err: nil,
		},
		{
			name: "test if w/ then & else statements",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.ELSE, Lexeme: "else", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				ThenBranch: &statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
				},
				ElseBranch: &statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 1},
					},
				},
			},
			err: nil,
		},
		{
			name: "test if w/ else statement",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.ELSE, Lexeme: "else", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.ELSE, Lexeme: "else", Line: 1},
				Message: "Expected expression but got else",
			},
		},
		{
			name: "test empty if condition",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test invalid if condition",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.GREATER, Lexeme: ">", Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected expression but got )",
			},
		},
		{
			name: "test if w/ no then & else statements",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected expression but got ",
			},
		},
		{
			name: "test if statement w/ expression as condition",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.If{
				Condition: &expression.Binary{
					Left: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
				ThenBranch: &statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
				},
			},
			err: nil,
		},
		{
			name: "test if statement w/ no right parenthesis",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				Message: "Expected \")\" after if condition",
			},
		},
		{
			name: "test if statement w/ no left parenthesis",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.TRUE, Lexeme: "true", Line: 1},
				Message: "Expected \"(\" after \"if\"",
			},
		},
		{
			name: "test nested if statements",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: literal.GenerateBoolLiteral(false), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				ThenBranch: &statement.If{
					Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					ThenBranch: &statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
						},
					},
				},
			},
			err: nil,
		},
		{
			name: "test nested if statements with inner else",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: literal.GenerateBoolLiteral(false), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.ELSE, Lexeme: "else", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				ThenBranch: &statement.If{
					Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					ThenBranch: &statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
						},
					},
					ElseBranch: &statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 1},
						},
					},
				},
			},
			err: nil,
		},
		{
			name: "test nested if statements with else",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: literal.GenerateBoolLiteral(false), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.ELSE, Lexeme: "else", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.ELSE, Lexeme: "else", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "c", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				ThenBranch: &statement.If{
					Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					ThenBranch: &statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
						},
					},
					ElseBranch: &statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 1},
						},
					},
				},
				ElseBranch: &statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "c", Line: 1},
					},
				},
			},
			err: nil,
		},
		{
			name: "test nested if statements with outer else",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.TRUE, Lexeme: "true", Literal: literal.GenerateBoolLiteral(true), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.LEFT_BRACE, Lexeme: "{", Line: 1},
				{TokenType: token.IF, Lexeme: "if", Line: 1},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Line: 1},
				{TokenType: token.FALSE, Lexeme: "false", Literal: literal.GenerateBoolLiteral(false), Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.RIGHT_BRACE, Lexeme: "}", Line: 1},
				{TokenType: token.ELSE, Lexeme: "else", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				ThenBranch: &statement.Block{
					Statements: []statement.Stmt{
						&statement.If{
							Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
							ThenBranch: &statement.Print{
								Expression: &expression.Variable{
									Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
								},
							},
						},
					},
				},
				ElseBranch: &statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 1},
					},
				},
			},
			err: nil,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.statement()
			if test.err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestVarDeclaration(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected statement.Stmt
		err      error
	}

	tests := []testCase{
		{
			name: "test initialize variable w/ no initializer",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LET, Lexeme: "let", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Let{
				Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Initializer: nil,
			},
			err: nil,
		},
		{
			name: "test initialize variable w/ literal initializer",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LET, Lexeme: "let", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Let{
				Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
			},
			err: nil,
		},
		{
			name: "test initialize variable w/ expression initializer",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LET, Lexeme: "let", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Let{
				Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Initializer: &expression.Binary{
					Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
			},
			err: nil,
		},
		{
			name: "test unterminated variable declaration",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LET, Lexeme: "let", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: literal.GenerateNumericLiteral(1), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected \";\" after variable declaration.",
			},
		},
		{
			name: "test incomplete variable definition",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LET, Lexeme: "let", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				Message: "Expected expression but got ;",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.declaration()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestBlock(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected statement.Stmt
		err      error
	}

	tests := []testCase{
		// Add to TestBlock:
		{
			name: "test empty block",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LEFT_BRACE, Lexeme: "{", Line: 1},
				{TokenType: token.RIGHT_BRACE, Lexeme: "}", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Block{Statements: []statement.Stmt{}},
			err:      nil,
		},
		{
			name: "test one-line block",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LEFT_BRACE, Lexeme: "{", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.RIGHT_BRACE, Lexeme: "}", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Block{Statements: []statement.Stmt{
				&statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
				},
			}},
			err: nil,
		},
		{
			name: "test multiple lines block",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LEFT_BRACE, Lexeme: "{", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 2},
				{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 2},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 2},
				{TokenType: token.PRINT, Lexeme: "print", Line: 3},
				{TokenType: token.IDENTIFIER, Lexeme: "c", Line: 3},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 3},
				{TokenType: token.RIGHT_BRACE, Lexeme: "}", Line: 4},
				{TokenType: token.EOF, Lexeme: "", Line: 4},
			}, Curr: 0},
			expected: &statement.Block{Statements: []statement.Stmt{
				&statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
					},
				},
				&statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b", Line: 2},
					},
				},
				&statement.Print{
					Expression: &expression.Variable{
						Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "c", Line: 3},
					},
				},
			}},
			err: nil,
		},
		{
			name: "test unterminated block",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.LEFT_BRACE, Lexeme: "{", Line: 1},
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected \"}\" after block.",
			},
		},
		// FIXME: Add test for missing left braces
		// {
		//     name: "test missing left braces",
		//     parser: Parser{Tokens: []token.Token{
		//         {TokenType: token.PRINT, Lexeme: "print", Line: 1},
		//         {TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
		//         {TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
		//         {TokenType: token.RIGHT_BRACE, Lexeme: "}", Line: 1},
		//         {TokenType: token.EOF, Lexeme: "", Line: 1},
		//     }, Curr: 0},
		//     expected: nil,
		//     err: &exu_err.SyntaxError{
		//         Token: token.Token{TokenType: token.RIGHT_BRACE, Lexeme: "}", Line: 1},
		//         Message: "Unexpected }",
		//     },
		// },
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.statement()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestPrintStatement(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected statement.Stmt
		err      error
	}

	tests := []testCase{
		{
			name: "test valid print statement w/ literal",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Print{
				Expression: &expression.Literal{Value: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}()},
			},
			err: nil,
		},
		{
			name: "test valid print statement w/ expression",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
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
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Print{
				Expression: &expression.Binary{
					Left: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
					Right: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
				},
			},
			err: nil,
		},
		{
			name: "test unterminated print statement",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected \";\" after expression.",
			},
		},
		{
			name: "test invalid expression in print statement",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.PRINT, Lexeme: "print", Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				Message: "Expected expression but got ;",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Run statement method to include print keyword in parse testing
			actual, err := test.parser.statement()
			if test.err != nil || err != nil {
				// assert.EqualError(t, err, test.err.Error())
				assert.Equal(t, test.err, err)
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestExprStatement(t *testing.T) {
	type testCase struct {
		name   string
		parser Parser

		expected statement.Stmt
		err      error
	}

	tests := []testCase{
		{
			name: "test valid expr statement",
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
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: &statement.Expression{
				Expression: &expression.Binary{
					Left: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
					Right: &expression.Literal{Value: func() *literal.NumericLiteral {
						lit := &literal.NumericLiteral{}
						lit.SetVal(1)
						return lit
					}()},
				},
			},
			err: nil,
		},
		{
			name: "test unterminated expr statement",
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
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.EOF, Lexeme: "", Line: 1},
				Message: "Expected \";\" after expression.",
			},
		},
		{
			name: "test invalid expression in expr statement",
			parser: Parser{Tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(1)
					return lit
				}(), Line: 1},
				{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				{TokenType: token.EOF, Lexeme: "", Line: 1},
			}, Curr: 0},
			expected: nil,
			err: &exu_err.SyntaxError{
				Token:   token.Token{TokenType: token.SEMICOLON, Lexeme: ";", Line: 1},
				Message: "Expected expression but got ;",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.exprStatement()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}
