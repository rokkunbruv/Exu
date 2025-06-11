package token

import (
	"testing"

	"github.com/rokkunbruv/internals/literal"
	"github.com/stretchr/testify/assert"
)

func TestTokenToString(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected string
	}{
		{
			name: "test valid token with string literal",
			token: Token{
				TokenType: IDENTIFIER,
				Lexeme:    "myVar",
				Literal: func() *literal.StringLiteral {
					lit := &literal.StringLiteral{}
					lit.SetVal("test")
					return lit
				}(),
				Line: 1,
			},
			expected: "IDENTIFIER myVar test",
		},
		{
			name: "test valid token with numeric literal",
			token: Token{
				TokenType: NUMERIC,
				Lexeme:    "42.5",
				Literal: func() *literal.NumericLiteral {
					lit := &literal.NumericLiteral{}
					lit.SetVal(42.5)
					return lit
				}(),
				Line: 1,
			},
			expected: "NUMERIC 42.5 42.5",
		},
		{
			name: "test token with null literal",
			token: Token{
				TokenType: NULL,
				Lexeme:    "null",
				Literal: func() *literal.StringLiteral {
					return &literal.StringLiteral{IsNull: true}
				}(),
				Line: 1,
			},
			expected: "NULL null ",
		},
		{
			name:     "test zero value token",
			token:    Token{Literal: nil},
			expected: "",
		},
		{
			name: "test token with empty lexeme",
			token: Token{
				TokenType: EOF,
				Lexeme:    "",
				Literal: func() *literal.StringLiteral {
					return &literal.StringLiteral{IsNull: true}
				}(),
				Line: 0,
			},
			expected: "EOF  ",
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result := test.token.ToString()
			assert.Equal(t, test.expected, result)
		})
	}
}
