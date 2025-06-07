package lexer

import (
	"testing"

	"github.com/attic-labs/testify/assert"
	"github.com/rokkunbruv/internals/token"
)

func TestLexer(t *testing.T) {
	type testCase struct {
		source string

		expected []token.Token
		err      error
	}

	t.Run("Lexer", func(t *testing.T) {
		tests := []testCase{
			{
				source: "123",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 0},
				},
			},
			{
				source: "\"hello\"",
				expected: []token.Token{
					{TokenType: token.STRING, Lexeme: "\"hello\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "hello"}, Line: 0},
				},
			},
			{
				source: "if",
				expected: []token.Token{
					{TokenType: token.IF, Lexeme: "if", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				},
			},
			{
				source: "true false",
				expected: []token.Token{
					{TokenType: token.TRUE, Lexeme: "true", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.FALSE, Lexeme: "false", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				},
			},
			{
				source: "1 + 2",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "2", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 2}, Line: 0},
				},
			},
			{
				source: "x = 42",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EQUAL, Lexeme: "=", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "42", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 42}, Line: 0},
				},
			},
			{
				source: "# this is a comment\n123",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 1},
				},
			},
			{
				source: "!=",
				expected: []token.Token{
					{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				source: "<=",
				expected: []token.Token{
					{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				source: "fn test() { }",
				expected: []token.Token{
					{TokenType: token.FN, Lexeme: "fn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				source: "str: \"\";",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "str", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: ""}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				source: "str: \"a\";",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "str", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"a\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "a"}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
		}

		for _, test := range tests {
			actual, err := Lexer(test.source)
			assert.NoError(t, err)
			assert.Equal(t, test.expected, actual)

		}
	})
}
