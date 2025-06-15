package environment

import (
	"testing"

	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
	"github.com/stretchr/testify/assert"
)

func TestDefine(t *testing.T) {
	type testCase struct {
		name string
		env  Environment

		varName string
		value   literal.Literal

		expected Environment
	}

	tests := []testCase{
		{
			name:    "test define a to be 1",
			env:     GenerateEnvironment(nil),
			varName: "a",
			value:   literal.GenerateNumericLiteral(1),
			expected: Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateNumericLiteral(1),
				},
			},
		},
		{
			name: "test redefine a, previously a = 1, to be 2",
			env: Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateNumericLiteral(1),
				},
			},
			varName: "a",
			value:   literal.GenerateNumericLiteral(2),
			expected: Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateNumericLiteral(2),
				},
			},
		},
		{
			name: "test redefine a, previously a = 1, to be \"str\"",
			env: Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateNumericLiteral(1),
				},
			},
			varName: "a",
			value:   literal.GenerateStringLiteral("str"),
			expected: Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateStringLiteral("str"),
				},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			test.env.Define(test.varName, test.value)
			assert.Equal(t, test.expected, test.env)
		})
	}
}

func TestGet(t *testing.T) {
	type testCase struct {
		name string
		env  Environment

		variable token.Token

		expected literal.Literal
		err      error
	}

	tests := []testCase{
		{
			name: "test get value of a = 1",
			env: Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateNumericLiteral(1),
				},
			},
			variable: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
			expected: literal.GenerateNumericLiteral(1),
			err:      nil,
		},
		{
			name:     "test get value of uninitialized a",
			env:      GenerateEnvironment(nil),
			variable: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
			expected: nil,
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Message: "Cannot get value of undefined variable \"a\"",
			},
		},
		{
			name: "test get value of a = 1 from enclosing",
			env: GenerateEnvironment(&Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateNumericLiteral(1),
				},
			}),
			variable: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
			expected: literal.GenerateNumericLiteral(1),
			err:      nil,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.env.Get(test.variable)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestAssign(t *testing.T) {
	type testCase struct {
		name string
		env  Environment

		variable token.Token
		value    literal.Literal

		expected Environment
		err      error
	}

	tests := []testCase{{
		name: "test assign a = 1 to be 2",
		env: Environment{
			values: map[string]literal.Literal{
				"a": literal.GenerateNumericLiteral(1),
			},
		},
		variable: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
		value:    literal.GenerateNumericLiteral(2),
		expected: Environment{
			values: map[string]literal.Literal{
				"a": literal.GenerateNumericLiteral(2),
			},
		},
		err: nil,
	},
		{
			name:     "test assign uninitialized a to be 1",
			env:      GenerateEnvironment(nil),
			variable: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
			value:    literal.GenerateNumericLiteral(1),
			expected: GenerateEnvironment(nil),
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Message: "Cannot assign to undefined variable \"a\"",
			},
		},
		{
			name: "test assign value of a = 1 from enclosing to be 2",
			env: GenerateEnvironment(&Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateNumericLiteral(1),
				},
			}),
			variable: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
			value:    literal.GenerateNumericLiteral(2),
			expected: GenerateEnvironment(&Environment{
				values: map[string]literal.Literal{
					"a": literal.GenerateNumericLiteral(2),
				},
			}),
			err: nil,
		}}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			err := test.env.Assign(test.variable, test.value)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, test.env)
		})
	}
}
