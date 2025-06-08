package lexer

import (
	"fmt"
	"testing"
	"unicode/utf8"

	"github.com/attic-labs/testify/assert"
	"github.com/rokkunbruv/internals/token"
)

func TestLexer(t *testing.T) {
	t.Parallel()

	type testCase struct {
		name string

		source string

		expected []token.Token
		err      error
	}

	tests := []testCase{
		{
			name:   "test all keywords, operators, and separators in one line",
			source: "fn test() { if true && false || null while for else class super self return print }",
			expected: []token.Token{
				{TokenType: token.FN, Lexeme: "fn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.IDENTIFIER, Lexeme: "test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.IF, Lexeme: "if", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.TRUE, Lexeme: "true", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.AND, Lexeme: "&", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.AND, Lexeme: "&", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.FALSE, Lexeme: "false", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.OR, Lexeme: "|", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.OR, Lexeme: "|", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NULL, Lexeme: "null", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.WHILE, Lexeme: "while", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.FOR, Lexeme: "for", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.ELSE, Lexeme: "else", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.CLASS, Lexeme: "class", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.SUPER, Lexeme: "super", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.SUPER, Lexeme: "self", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.RETURN, Lexeme: "return", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.PRINT, Lexeme: "print", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
			},
		},
		{
			name:   "test all operators and number formats",
			source: "1 + -2.34 * 0.5 / != <= >= < > == : <- 000123.456",
			expected: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
				{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.MINUS, Lexeme: "-", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NUMERIC, Lexeme: "2.34", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 2.34}, Line: 0},
				{TokenType: token.STAR, Lexeme: "*", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NUMERIC, Lexeme: "0.5", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 0.5}, Line: 0},
				{TokenType: token.SLASH, Lexeme: "/", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.LESS, Lexeme: "<", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.GREATER, Lexeme: ">", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NUMERIC, Lexeme: "000123.456", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123.456}, Line: 0},
				{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
			},
		},
		{
			name:     "test edge cases: unicode, invalid identifiers, string edge cases",
			source:   "id你好 1var .123 42. whiile \"test\n\" @#$",
			expected: nil,
			err:      fmt.Errorf("unexpected 你 found at line 0"),
		},
		{
			name:   "test whitespace handling and comment",
			source: "fn\ttest\n(\n)\t{\n#comment\n}",
			expected: []token.Token{
				{TokenType: token.FN, Lexeme: "fn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.IDENTIFIER, Lexeme: "test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 1},
				{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 2},
				{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 2},
				{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 4},
				{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 4},
			},
		},
		{
			name:     "test string handling and escapes",
			source:   "str: \"hello\nworld\" \"unterminated",
			expected: nil,
			err:      fmt.Errorf("unterminated string at line 1"),
		},
		{
			name:   "test adjacent tokens and special identifiers",
			source: "x:1,_y:.2,z:",
			expected: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
				{TokenType: token.COMMA, Lexeme: ",", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.IDENTIFIER, Lexeme: "_y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NUMERIC, Lexeme: "2", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 2}, Line: 0},
				{TokenType: token.COMMA, Lexeme: ",", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.IDENTIFIER, Lexeme: "z", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
			},
		},
		{
			name:     "test unterminated strings and string escapes",
			source:   "str1: \"hello\nstr2: \"world\n\nstr3: \"incomplete",
			expected: nil,
			err:      fmt.Errorf("unterminated string at line 3"),
		},
		{
			name:   "test adjacent comparison operators",
			source: "!=!<==>=><==<>=!==",
			expected: []token.Token{
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NOT, Lexeme: "!", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.GREATER, Lexeme: ">", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.LESS, Lexeme: "<", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.EQUAL, Lexeme: "=", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
			},
		},
		{
			name:   "test invalid identifiers and comments across multiple lines",
			source: "1var_test: 42\n_valid_var: 10\n#comment line 1\n#comment line 2\n123_invalid: 30",
			expected: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
				{TokenType: token.IDENTIFIER, Lexeme: "var_test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
				{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
				{TokenType: token.NUMERIC, Lexeme: "42", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 42}, Line: 0},
				{TokenType: token.IDENTIFIER, Lexeme: "_valid_var", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 1},
				{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "10", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 10}, Line: 1},
				{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 4},
				{TokenType: token.IDENTIFIER, Lexeme: "_invalid", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 4},
				{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 4},
				{TokenType: token.NUMERIC, Lexeme: "30", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 30}, Line: 4},
				{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 4},
			},
		},
		{
			name:   "test comments with various content and empty lines",
			source: "#comment with numbers 123\n\n#comment with symbols @#$\n#comment with keywords if while\n",
			expected: []token.Token{
				{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 4},
			},
		},
		{
			name:     "test mixed string termination and escape sequences",
			source:   "str1: \"valid\n\" + str2: \"invalid\n\" + str3: \"unterminated",
			expected: nil,
			err:      fmt.Errorf("unterminated string at line 2"),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := Lexer(test.source)
			if test.err != nil {
				assert.Equal(t, test.err, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, test.expected, actual)
			}
		})
	}
}

func TestPeek(t *testing.T) {
	type testCase struct {
		name string

		source string
		curr   int

		expected rune
	}

	tests := []testCase{
		{
			name:     "test peek on index 0",
			source:   "hello",
			curr:     0,
			expected: 'e',
		},
		{
			name:     "test peek at end of string",
			source:   "hello",
			curr:     4,
			expected: 0,
		},
		{
			name:     "test peek beyond string length",
			source:   "hello",
			curr:     5,
			expected: 0,
		},
		{
			name:     "test peek next unicode character",
			source:   "你好",
			curr:     0,
			expected: '好',
		},
		{
			name:     "test peek in middle of UTF-unicode character bytes",
			source:   "你好",
			curr:     1,
			expected: utf8.RuneError,
		},
		{
			name:     "test peek to ascii character on unicode character",
			source:   "🌍world",
			curr:     0,
			expected: 'w',
		},
		{
			name:     "test peek to ascii character in the middle of unicode character bytes",
			source:   "🌍world",
			curr:     utf8.RuneError,
			expected: 0,
		},
		{
			name:     "test peek to whitespace on unicode character",
			source:   "π = 3.14",
			curr:     0,
			expected: ' ',
		},
		{
			name:     "test peek on empty string",
			source:   "",
			curr:     0,
			expected: 0,
		},
		{
			name:     "test peek to unicode character on ascii",
			source:   "hello世界",
			curr:     4,
			expected: '世',
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual := peek(test.source, test.curr)
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestAddToken(t *testing.T) {
	type testCase struct {
		name string

		tokens    []token.Token
		tokenType token.TokenType
		lexeme    string
		literal   token.Literal
		line      int

		expected []token.Token
	}

	tests := []testCase{
		{
			name:      "test add numeric token to empty list",
			tokens:    []token.Token{},
			tokenType: token.NUMERIC,
			lexeme:    "123",
			literal:   token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123},
			line:      0,
			expected: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 0},
			},
		},
		{
			name: "test add operator after numeric",
			tokens: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 0},
			},
			tokenType: token.PLUS,
			lexeme:    "+",
			literal:   token.Literal{IsNull: true},
			line:      0,
			expected: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 0},
				{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
			},
		},
		{
			name:      "test add string token",
			tokens:    []token.Token{},
			tokenType: token.STRING,
			lexeme:    "\"hello\"",
			literal:   token.Literal{Type: token.STRING_LITERAL, StrVal: "hello"},
			line:      0,
			expected: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"hello\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "hello"}, Line: 0},
			},
		},
		{
			name:      "test add keyword token",
			tokens:    []token.Token{},
			tokenType: token.IF,
			lexeme:    "if",
			literal:   token.Literal{Type: token.STRING_LITERAL, IsNull: true},
			line:      0,
			expected: []token.Token{
				{TokenType: token.IF, Lexeme: "if", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
			},
		},
		{
			name:      "test add token with different line number",
			tokens:    []token.Token{},
			tokenType: token.NUMERIC,
			lexeme:    "456",
			literal:   token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 456},
			line:      2,
			expected: []token.Token{
				{TokenType: token.NUMERIC, Lexeme: "456", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 456}, Line: 2},
			},
		},
		{
			name:      "test add boolean token",
			tokens:    []token.Token{},
			tokenType: token.TRUE,
			lexeme:    "true",
			literal:   token.Literal{Type: token.STRING_LITERAL, IsNull: true},
			line:      0,
			expected: []token.Token{
				{TokenType: token.TRUE, Lexeme: "true", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
			},
		},
		{
			name:      "test add identifier token",
			tokens:    []token.Token{},
			tokenType: token.IDENTIFIER,
			lexeme:    "x",
			literal:   token.Literal{Type: token.STRING_LITERAL, IsNull: true},
			line:      0,
			expected: []token.Token{
				{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
			},
		},
		{
			name:      "test add empty string token",
			tokens:    []token.Token{},
			tokenType: token.STRING,
			lexeme:    "\"\"",
			literal:   token.Literal{Type: token.STRING_LITERAL, StrVal: ""},
			line:      0,
			expected: []token.Token{
				{TokenType: token.STRING, Lexeme: "\"\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: ""}, Line: 0},
			},
		},
		{
			name:      "test add function keyword token",
			tokens:    []token.Token{},
			tokenType: token.FN,
			lexeme:    "fn",
			literal:   token.Literal{Type: token.STRING_LITERAL, IsNull: true},
			line:      0,
			expected: []token.Token{
				{TokenType: token.FN, Lexeme: "fn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
			},
		},
		{
			name:      "test add comparison operator token",
			tokens:    []token.Token{},
			tokenType: token.LESS_EQUAL,
			lexeme:    "<=",
			literal:   token.Literal{IsNull: true},
			line:      0,
			expected: []token.Token{
				{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: token.Literal{IsNull: true}, Line: 0},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			addToken(&test.tokens, test.tokenType, test.lexeme, test.literal, test.line)
			assert.Equal(t, test.expected, test.tokens)
		})
	}
}

func TestGetLexeme(t *testing.T) {
	type testCase struct {
		name string

		source string
		start  int
		curr   int

		expected string
	}

	tests := []testCase{
		{
			name:     "test on first character",
			source:   "hello",
			start:    0,
			curr:     1,
			expected: "h",
		},
		{
			name:     "test on whole word",
			source:   "hello",
			start:    0,
			curr:     5,
			expected: "hello",
		},
		{
			name:     "test on first unicode character (chinese character, which should have 3 bytes each)",
			source:   "你好",
			start:    0,
			curr:     3,
			expected: "你",
		},
		{
			name:     "test on another first unicode character (emoji, which should have 4 bytes each)",
			source:   "🌍world",
			start:    0,
			curr:     4,
			expected: "🌍",
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual := getLexeme(test.source, test.start, test.curr)
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestIsAtEnd(t *testing.T) {
	type testCase struct {
		name string

		source string
		curr   int

		expected bool
	}

	tests := []testCase{
		{
			name:     "test on beginning of string",
			source:   "hello",
			curr:     0,
			expected: false,
		},
		{
			name:     "test at end of string",
			source:   "hello",
			curr:     5,
			expected: true,
		},
		{
			name:     "test empty string",
			source:   "",
			curr:     0,
			expected: true,
		},
		{
			name:     "test unicode character character",
			source:   "你好",
			curr:     2,
			expected: true,
		},
		{
			name:     "test mix of unicode and ascii characters",
			source:   "🌍world",
			curr:     6,
			expected: true,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual := isAtEnd(test.source, test.curr)
			assert.Equal(t, test.expected, actual)
		})
	}
}
