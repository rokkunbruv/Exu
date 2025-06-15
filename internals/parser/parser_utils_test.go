package parser

import (
	"testing"

	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
	"github.com/stretchr/testify/assert"
)

func TestMatch(t *testing.T) {
	tokens := generateTestTokens()

	type testCase struct {
		name   string
		parser Parser

		types []token.TokenType

		expected bool
		err      error
	}

	tests := []testCase{
		{
			name:     "test empty tokens list",
			parser:   Parser{Tokens: []token.Token{}, Curr: 0},
			types:    []token.TokenType{token.PLUS, token.MINUS},
			expected: false,
			err:      &exu_err.ParseError{Curr: 0, Message: "Index out of bounds"},
		},
		{
			name:     "test curr at start and all types match",
			parser:   Parser{Tokens: tokens, Curr: 0},
			types:    []token.TokenType{token.NUMERIC},
			expected: true,
			err:      nil,
		},
		{
			name:     "test curr in middle and all types match",
			parser:   Parser{Tokens: tokens, Curr: 5},
			types:    []token.TokenType{token.NUMERIC},
			expected: true,
			err:      nil,
		},
		{
			name:     "test curr at end and all types match",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) - 1},
			types:    []token.TokenType{token.EOF},
			expected: false,
			err:      nil,
		},
		{
			name:     "test curr in middle and no types match",
			parser:   Parser{Tokens: tokens, Curr: 5},
			types:    []token.TokenType{token.PLUS, token.MINUS},
			expected: false,
			err:      nil,
		},
		{
			name:     "test curr at end and no types match",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) - 1},
			types:    []token.TokenType{token.PLUS, token.MINUS},
			expected: false,
			err:      nil,
		},
		{
			name:     "test curr in middle and some types match",
			parser:   Parser{Tokens: tokens, Curr: 1},
			types:    []token.TokenType{token.PLUS, token.MINUS, token.STAR},
			expected: true,
			err:      nil,
		},
		{
			name:     "test curr at end and some types match",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) - 1},
			types:    []token.TokenType{token.PLUS, token.EOF, token.STAR},
			expected: false,
			err:      nil,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.match(test.types...)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestConsume(t *testing.T) {
	tokens := generateTestTokens()

	type testCase struct {
		name   string
		parser Parser

		tokenType token.TokenType
		errorMsg  string

		expected token.Token
		err      error
	}

	tests := []testCase{
		{
			name:      "test empty tokens list",
			parser:    Parser{Tokens: []token.Token{}, Curr: 0},
			tokenType: token.PLUS,
			errorMsg:  "expected plus token",
			expected:  token.Token{},
			err:       &exu_err.ParseError{Curr: 0, Message: "Index out of bounds"},
		},
		{
			name:      "test curr at start and type matches",
			parser:    Parser{Tokens: tokens, Curr: 0},
			tokenType: token.NUMERIC,
			errorMsg:  "expected numeric token",
			expected:  tokens[0],
			err:       nil,
		},
		{
			name:      "test curr in middle and type matches",
			parser:    Parser{Tokens: tokens, Curr: 5},
			tokenType: token.NUMERIC,
			errorMsg:  "expected numeric token",
			expected:  tokens[5],
			err:       nil,
		},
		{
			name:      "test curr at end and type matches",
			parser:    Parser{Tokens: tokens, Curr: len(tokens) - 1},
			tokenType: token.EOF,
			errorMsg:  "expected EOF token",
			expected:  token.Token{},
			err: &exu_err.SyntaxError{
				Token:   tokens[len(tokens)-1],
				Message: "expected EOF token",
			},
		},
		{
			name:      "test curr in middle and type not matches",
			parser:    Parser{Tokens: tokens, Curr: 5},
			tokenType: token.PLUS,
			errorMsg:  "expected plus token",
			expected:  token.Token{},
			err: &exu_err.SyntaxError{
				Token:   tokens[5],
				Message: "expected plus token",
			},
		},
		{
			name:      "test curr at end and type not matches",
			parser:    Parser{Tokens: tokens, Curr: len(tokens) - 1},
			tokenType: token.PLUS,
			errorMsg:  "expected plus token",
			expected:  token.Token{},
			err: &exu_err.SyntaxError{
				Token:   tokens[len(tokens)-1],
				Message: "expected plus token",
			},
		},
		{
			name:      "test empty error message",
			parser:    Parser{Tokens: tokens, Curr: 5},
			tokenType: token.PLUS,
			errorMsg:  "",
			expected:  token.Token{},
			err: &exu_err.SyntaxError{
				Token:   tokens[5],
				Message: "",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.consume(test.tokenType, test.errorMsg)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestCheck(t *testing.T) {
	tokens := generateTestTokens()

	type testCase struct {
		name   string
		parser Parser

		tokenType token.TokenType

		expected bool
		err      error
	}

	tests := []testCase{
		{
			name:      "test empty tokens list",
			parser:    Parser{Tokens: []token.Token{}, Curr: 0},
			tokenType: token.PLUS,
			expected:  false,
			err:       &exu_err.ParseError{Curr: 0, Message: "Index out of bounds"},
		},
		{
			name:      "test curr at start and type matches",
			parser:    Parser{Tokens: tokens, Curr: 0},
			tokenType: token.NUMERIC,
			expected:  true,
			err:       nil,
		},
		{
			name:      "test curr in middle and type matches",
			parser:    Parser{Tokens: tokens, Curr: 5},
			tokenType: token.NUMERIC,
			expected:  true,
			err:       nil,
		},
		{
			name:      "test curr at end and type matches",
			parser:    Parser{Tokens: tokens, Curr: len(tokens) - 1},
			tokenType: token.EOF,
			expected:  false,
			err:       nil,
		},
		{
			name:      "test curr in middle and type not matches",
			parser:    Parser{Tokens: tokens, Curr: 5},
			tokenType: token.PLUS,
			expected:  false,
			err:       nil,
		},
		{
			name:      "test curr at end and type not matches",
			parser:    Parser{Tokens: tokens, Curr: len(tokens) - 1},
			tokenType: token.PLUS,
			expected:  false,
			err:       nil,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.check(test.tokenType)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestAdvance(t *testing.T) {
	tokens := generateTestTokens()

	type testCase struct {
		name   string
		parser Parser

		expected token.Token
		err      error
	}

	tests := []testCase{
		{
			name:     "test empty tokens list",
			parser:   Parser{Tokens: []token.Token{}, Curr: 0},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: 0, Message: "Index out of bounds"},
		},
		{
			name:     "test curr at start",
			parser:   Parser{Tokens: tokens, Curr: 0},
			expected: tokens[0],
			err:      nil,
		},
		{
			name:     "test curr at arbitrary position",
			parser:   Parser{Tokens: tokens, Curr: 3},
			expected: tokens[3],
			err:      nil,
		},
		{
			name:     "test curr at end",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) - 1},
			expected: tokens[len(tokens)-2],
			err:      nil,
		},
		{
			name:     "test curr at negative index",
			parser:   Parser{Tokens: tokens, Curr: -1},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: -1, Message: "Index out of bounds"},
		},
		{
			name:     "test curr beyond tokens length",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) + 1},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: len(tokens) + 1, Message: "Index out of bounds"},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.advance()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestIsAtEnd(t *testing.T) {
	tokens := generateTestTokens()

	type testCase struct {
		name   string
		parser Parser

		expected bool
		err      error
	}

	tests := []testCase{
		{
			name:     "test empty tokens list",
			parser:   Parser{Tokens: []token.Token{}, Curr: 0},
			expected: false,
			err:      &exu_err.ParseError{Curr: 0, Message: "Index out of bounds"},
		},
		{
			name:     "test curr at start",
			parser:   Parser{Tokens: tokens, Curr: 0},
			expected: false,
		},
		{
			name:     "test curr at middle",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) / 2},
			expected: false,
		},
		{
			name:     "test curr at end",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) - 1},
			expected: true,
		},
		{
			name:     "test curr at negative index",
			parser:   Parser{Tokens: tokens, Curr: -1},
			expected: false,
			err:      &exu_err.ParseError{Curr: -1, Message: "Index out of bounds"},
		},
		{
			name:     "test curr beyond tokens length",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) + 1},
			expected: false,
			err:      &exu_err.ParseError{Curr: len(tokens) + 1, Message: "Index out of bounds"},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.isAtEnd()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestPeek(t *testing.T) {
	tokens := generateTestTokens()

	type testCase struct {
		name   string
		parser Parser

		expected token.Token
		err      error
	}

	tests := []testCase{
		{
			name:     "test empty tokens list",
			parser:   Parser{Tokens: []token.Token{}, Curr: 0},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: 0, Message: "Index out of bounds"},
		},
		{
			name:     "test curr at middle",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) / 2},
			expected: tokens[len(tokens)/2],
			err:      nil,
		},
		{
			name:     "test curr at end",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) - 1},
			expected: tokens[len(tokens)-1],
			err:      nil,
		},
		{
			name:     "test curr at negative index",
			parser:   Parser{Tokens: tokens, Curr: -1},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: -1, Message: "Index out of bounds"},
		},
		{
			name:     "test curr beyond tokens length",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) + 1},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: len(tokens) + 1, Message: "Index out of bounds"},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.peek()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestPrevious(t *testing.T) {
	tokens := generateTestTokens()

	type testCase struct {
		name   string
		parser Parser

		expected token.Token
		err      error
	}

	tests := []testCase{
		{
			name:     "test empty tokens list",
			parser:   Parser{Tokens: []token.Token{}, Curr: 0},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: -1, Message: "Index out of bounds"},
		},
		{
			name:     "test curr at start",
			parser:   Parser{Tokens: tokens, Curr: 0},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: -1, Message: "Index out of bounds"},
		},
		{
			name:     "test curr at middle",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) / 2},
			expected: tokens[len(tokens)/2-1],
			err:      nil,
		},
		{
			name:     "test curr at negative index",
			parser:   Parser{Tokens: tokens, Curr: -1},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: -2, Message: "Index out of bounds"},
		},
		{
			name:     "test curr beyond tokens length",
			parser:   Parser{Tokens: tokens, Curr: len(tokens) + 1},
			expected: token.Token{},
			err:      &exu_err.ParseError{Curr: len(tokens), Message: "Index out of bounds"},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.parser.previous()
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

// Utility function to generate a sample tokens list
func generateTestTokens() []token.Token {
	return []token.Token{
		{TokenType: token.NUMERIC, Lexeme: "1", Literal: func() *literal.NumericLiteral {
			lit := &literal.NumericLiteral{}
			lit.SetVal(1)
			return lit
		}(), Line: 0},
		{TokenType: token.PLUS, Lexeme: "+", Literal: nil, Line: 0},
		{TokenType: token.MINUS, Lexeme: "-", Literal: nil, Line: 0},
		{TokenType: token.NUMERIC, Lexeme: "2.34", Literal: func() *literal.NumericLiteral {
			lit := &literal.NumericLiteral{}
			lit.SetVal(2.34)
			return lit
		}(), Line: 0},
		{TokenType: token.STAR, Lexeme: "*", Literal: nil, Line: 0},
		{TokenType: token.NUMERIC, Lexeme: "0.5", Literal: func() *literal.NumericLiteral {
			lit := &literal.NumericLiteral{}
			lit.SetVal(0.5)
			return lit
		}(), Line: 0},
		{TokenType: token.SLASH, Lexeme: "/", Literal: nil, Line: 0},
		{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: nil, Line: 0},
		{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: nil, Line: 0},
		{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: nil, Line: 0},
		{TokenType: token.LESS, Lexeme: "<", Literal: nil, Line: 0},
		{TokenType: token.GREATER, Lexeme: ">", Literal: nil, Line: 0},
		{TokenType: token.EQUAL, Lexeme: "=", Literal: nil, Line: 0},
		{TokenType: token.COLON, Lexeme: ":", Literal: nil, Line: 0},
		{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: nil, Line: 0},
		{TokenType: token.NUMERIC, Lexeme: "000123.456", Literal: func() *literal.NumericLiteral {
			lit := &literal.NumericLiteral{}
			lit.SetVal(123.456)
			return lit
		}(), Line: 0},
		{TokenType: token.EOF, Lexeme: "", Literal: nil, Line: 0},
	}
}
