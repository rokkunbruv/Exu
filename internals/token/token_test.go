package token

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLiteralToString(t *testing.T) {
	tests := []struct {
		name     string
		literal  Literal
		expected string
		err      error
	}{
		{
			name: "test valid string literal",
			literal: Literal{
				Type:      STRING_LITERAL,
				StrVal:    "test",
				DoubleVal: 0,
				IsNull:    false,
			},
			expected: "test",
			err:      nil,
		},
		{
			name: "test valid numeric literal",
			literal: Literal{
				Type:      NUMERIC_LITERAL,
				StrVal:    "",
				DoubleVal: 42.5,
				IsNull:    false,
			},
			expected: "42.5",
			err:      nil,
		},
		{
			name: "test null literal",
			literal: Literal{
				Type:      STRING_LITERAL,
				StrVal:    "",
				DoubleVal: 0,
				IsNull:    true,
			},
			expected: "null",
			err:      nil,
		},
		{
			name: "test string type with garbage numeric value",
			literal: Literal{
				Type:      STRING_LITERAL,
				StrVal:    "hello",
				DoubleVal: 123.456,
				IsNull:    false,
			},
			expected: "hello",
			err:      nil,
		},
		{
			name: "test numeric type with garbage string value",
			literal: Literal{
				Type:      NUMERIC_LITERAL,
				StrVal:    "garbage",
				DoubleVal: 3.14,
				IsNull:    false,
			},
			expected: "3.14",
			err:      nil,
		},
		{
			name: "test isNull true with valid values",
			literal: Literal{
				Type:      STRING_LITERAL,
				StrVal:    "should be ignored",
				DoubleVal: 42,
				IsNull:    true,
			},
			expected: "null",
			err:      nil,
		},
		{
			name: "test invalid literal type",
			literal: Literal{
				Type:      literalType(999),
				StrVal:    "",
				DoubleVal: 0,
				IsNull:    false,
			},
			expected: "",
			err:      fmt.Errorf("invalid literal"),
		},
		{
			name:     "Zero values",
			literal:  Literal{},
			expected: "",
			err:      nil,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result, err := test.literal.ToString()
			if test.err != nil {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, test.expected, result)
			}
		})
	}
}

func TestTokenToString(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected string
		err      error
	}{
		{
			name: "test valid token with string literal",
			token: Token{
				TokenType: IDENTIFIER,
				Lexeme:    "myVar",
				Literal: Literal{
					Type:   STRING_LITERAL,
					StrVal: "test",
				},
				Line: 1,
			},
			expected: "IDENTIFIER myVar test",
			err:      nil,
		},
		{
			name: "test valid token with numeric literal",
			token: Token{
				TokenType: NUMERIC,
				Lexeme:    "42.5",
				Literal: Literal{
					Type:      NUMERIC_LITERAL,
					DoubleVal: 42.5,
				},
				Line: 1,
			},
			expected: "NUMERIC 42.5 42.5",
			err:      nil,
		},
		{
			name: "test token with null literal",
			token: Token{
				TokenType: NULL,
				Lexeme:    "null",
				Literal: Literal{
					IsNull: true,
				},
				Line: 1,
			},
			expected: "NULL null null",
			err:      nil,
		},
		{
			name: "test token with invalid literal type",
			token: Token{
				TokenType: IDENTIFIER,
				Lexeme:    "test",
				Literal: Literal{
					Type: literalType(999),
				},
				Line: 1,
			},
			expected: "",
			err:      fmt.Errorf("invalid literal"),
		},
		{
			name:     "test zero value token",
			token:    Token{},
			expected: "",
			err:      nil,
		},
		{
			name: "test token with mismatched literal values",
			token: Token{
				TokenType: STRING,
				Lexeme:    "str",
				Literal: Literal{
					Type:      STRING_LITERAL,
					StrVal:    "hello",
					DoubleVal: 42,
				},
				Line: 1,
			},
			expected: "STRING str hello",
			err:      nil,
		},
		{
			name: "test token with empty lexeme",
			token: Token{
				TokenType: EOF,
				Lexeme:    "",
				Literal: Literal{
					IsNull: true,
				},
				Line: 0,
			},
			expected: "EOF  null",
			err:      nil,
		},
		{
			name: "test token with garbage line number",
			token: Token{
				TokenType: NUMERIC,
				Lexeme:    "42",
				Literal: Literal{
					Type:      NUMERIC_LITERAL,
					DoubleVal: 42,
				},
				Line: -1,
			},
			expected: "NUMERIC 42 42",
			err:      nil,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result, err := test.token.ToString()
			if test.err != nil {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, test.expected, result)
			}
		})
	}
}
