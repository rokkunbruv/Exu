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
		source string

		expected []token.Token
		err      error
	}

	t.Run("Lexer", func(t *testing.T) {
		tests := []testCase{
			{
				// Test for numeric literal
				source: "123",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for string literal
				source: "\"hello\"",
				expected: []token.Token{
					{TokenType: token.STRING, Lexeme: "\"hello\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "hello"}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for if keyword
				source: "if",
				expected: []token.Token{
					{TokenType: token.IF, Lexeme: "if", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for true and false keywords
				source: "true false",
				expected: []token.Token{
					{TokenType: token.TRUE, Lexeme: "true", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.FALSE, Lexeme: "false", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for addition between two single-digit numbers
				source: "1 + 2",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "2", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 2}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for variable comparison with a number
				source: "x = 42",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EQUAL, Lexeme: "=", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "42", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 42}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for comment and digit
				source: "# this is a comment\n123",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 1},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 1},
				},
			},
			{
				// Test for not equal operator
				source: "!=",
				expected: []token.Token{
					{TokenType: token.NOT_EQUAL, Lexeme: "!=", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for less than or equal operator
				source: "<=",
				expected: []token.Token{
					{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for function definition with an empty body
				source: "fn test() { }",
				expected: []token.Token{
					{TokenType: token.FN, Lexeme: "fn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for empty string initialization
				source: "str: \"\";",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "str", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: ""}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for single-character string initialization
				source: "str: \"a\";",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "str", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"a\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "a"}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for unclosed string literal with multiple lines
				source:   "\"hello\nworld",
				expected: nil,
				err:      fmt.Errorf("unterminated string at line 1"),
			},
			{
				// Test for function definition with irregular whitespaces and unclosed parenthesis
				source: "fn   test  (    x,y  {",
				expected: []token.Token{
					{TokenType: token.FN, Lexeme: "fn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COMMA, Lexeme: ",", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid operator sequence
				source: "+ - * / &&",
				expected: []token.Token{
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.MINUS, Lexeme: "-", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STAR, Lexeme: "*", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SLASH, Lexeme: "/", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.AND, Lexeme: "&", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.AND, Lexeme: "&", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for line starting with semicolon and invalid class declaration
				source: ";class<-{super.init}",
				expected: []token.Token{
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.CLASS, Lexeme: "class", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SUPER, Lexeme: "super", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "init", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for mixed numeric and string operations with invalid syntax
				source: "123\"hello\"456+",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"hello\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "hello"}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "456", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 456}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid variable assignment with multiple colons
				source: "x::42",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "42", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 42}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for incomplete if statement with mixed brackets
				source:   "if(x>y]}else{",
				expected: nil,
				err:      fmt.Errorf("unexpected ] found at line 0"),
			},
			{
				// Test for comment with no space after hash
				source: "#comment\nidentifier",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "identifier", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 1},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 1},
				},
			},
			{
				// Test for invalid numeric literals
				source: "123..456",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "123", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "456", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 456}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for mixed comparison operators
				source: "x>=><=<y",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.GREATER, Lexeme: ">", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LESS_EQUAL, Lexeme: "<=", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LESS, Lexeme: "<", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid class inheritance chain
				source: "class A<-B<-C",
				expected: []token.Token{
					{TokenType: token.CLASS, Lexeme: "class", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "A", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "B", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "C", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid string concatenation
				source: "\"hello\" + +\"world\"",
				expected: []token.Token{
					{TokenType: token.STRING, Lexeme: "\"hello\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "hello"}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"world\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "world"}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for malformed for loop
				source: "for(;i<5;;)",
				expected: []token.Token{
					{TokenType: token.FOR, Lexeme: "for", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "i", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LESS, Lexeme: "<", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "5", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 5}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for mixed logical operators
				source: "x&|!y",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.AND, Lexeme: "&", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.OR, Lexeme: "|", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NOT, Lexeme: "!", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid method call syntax
				source: "obj..method()",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "obj", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "method", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid use of self and super
				source: "self.super.self",
				expected: []token.Token{
					{TokenType: token.SUPER, Lexeme: "self", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SUPER, Lexeme: "super", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SUPER, Lexeme: "self", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid while loop condition
				source: "while(){}",
				expected: []token.Token{
					{TokenType: token.WHILE, Lexeme: "while", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid function return
				source: "return;;",
				expected: []token.Token{
					{TokenType: token.RETURN, Lexeme: "return", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for mixed valid and invalid syntax in class definition
				source: "class Test { init() self.x: 42 }",
				expected: []token.Token{
					{TokenType: token.CLASS, Lexeme: "class", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "Test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "init", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SUPER, Lexeme: "self", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "42", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 42}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for print statement with invalid concatenation
				source: "print \"a\" + + \"b\";",
				expected: []token.Token{
					{TokenType: token.PRINT, Lexeme: "print", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"a\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "a"}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"b\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "b"}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for multiple variable declarations with invalid syntax
				source: "x:1,y:,z:",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
					{TokenType: token.COMMA, Lexeme: ",", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.COMMA, Lexeme: ",", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "z", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for nested function calls with invalid spacing
				source: "foo(bar( baz(   )))",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "foo", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "bar", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "baz", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid numeric operations with multiple operators
				source: "1+-*/2",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.MINUS, Lexeme: "-", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STAR, Lexeme: "*", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.SLASH, Lexeme: "/", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "2", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 2}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for invalid identifier names with underscore
				source: "_123_abc_",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "_123_abc_", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for identifier starting with digit
				source: "1variable: 42",
				expected: []token.Token{
					{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "variable", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "42", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 42}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for unicode identifier
				source:   "变量: \"你好世界\"",
				expected: nil,
				err:      fmt.Errorf("unexpected 变 found at line 0"),
			},
			{
				// Test for emoji in string and identifier
				source:   "🌟var: \"Hello 🌍!\"",
				expected: nil,
				err:      fmt.Errorf("unexpected 🌟 found at line 0"),
			},
			{
				// Test for incomplete fractional number
				source: "num: 42.",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "num", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "42", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 42}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for number starting with dot
				source: "num: .42",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "num", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "42", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 42}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for tabs between tokens in function
				source: "fn\ttest\t(\tx,\ty\t)\t{\treturn\tx\t+\ty\t}",
				expected: []token.Token{
					{TokenType: token.FN, Lexeme: "fn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COMMA, Lexeme: ",", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RETURN, Lexeme: "return", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for newlines in function definition
				source: "fn\ntest\n(\nx,\ny\n)\n{\nreturn\n}\n",
				expected: []token.Token{
					{TokenType: token.FN, Lexeme: "fn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 1},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 2},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 3},
					{TokenType: token.COMMA, Lexeme: ",", Literal: token.Literal{IsNull: true}, Line: 3},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 4},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 5},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 6},
					{TokenType: token.RETURN, Lexeme: "return", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 7},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 8},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 9},
				},
			},
			{
				// Test for misspelled keywords
				source: "whiile (x > 0) { rreturn x }",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "whiile", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.GREATER, Lexeme: ">", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "0", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 0}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "rreturn", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for unknown characters mixed with valid syntax
				source:   "print @\"hello\" # ¥comment",
				expected: nil,
				err:      fmt.Errorf("unexpected @ found at line 0"),
			},
			{
				// Test for multiple empty lines between statements
				source: "x: 1\n\n\n\ny: 2",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 4},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 4},
					{TokenType: token.NUMERIC, Lexeme: "2", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 2}, Line: 4},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 4},
				},
			},
			{
				// Test for mixed unicode and ASCII in identifiers
				source:   "αβγ123_变量: \"test\"",
				expected: nil,
				err:      fmt.Errorf("unexpected α found at line 0"),
			},
			{
				// Test for excessive whitespace in class definition
				source: "class    Test     <-    Base    {    init   (  )    {    }    }",
				expected: []token.Token{
					{TokenType: token.CLASS, Lexeme: "class", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "Test", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_ARROW, Lexeme: "<-", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "Base", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "init", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for mixed tabs and spaces in if statement
				source: "if\t(x\t>\t0)\t{\n    return\tx\n\t}",
				expected: []token.Token{
					{TokenType: token.IF, Lexeme: "if", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.GREATER, Lexeme: ">", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "0", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 0}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RETURN, Lexeme: "return", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 1},
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 1},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 2},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for string with escape sequences and unicode
				source:   "str: \"Hello\\n世界\\t!\"",
				expected: nil,
				err:      fmt.Errorf("unexpected 世 found at line 0"),
			},
			{
				// Test for multiple dots in numeric literal
				source: "num: 1.2.3.4",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "num", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "1.2", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1.2}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "3", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 3}, Line: 0},
					{TokenType: token.DOT, Lexeme: ".", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "4", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 4}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for mixed case keywords
				source: "IF (True) { RETURN false }",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "IF", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "True", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.LEFT_BRACE, Lexeme: "{", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "RETURN", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.FALSE, Lexeme: "false", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_BRACE, Lexeme: "}", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for keywords as part of identifiers
				source: "whileLoop: true; ifCondition: false",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "whileLoop", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.TRUE, Lexeme: "true", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.SEMICOLON, Lexeme: ";", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "ifCondition", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.FALSE, Lexeme: "false", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for string with multiple line breaks
				source: "str: \"line1\nline2\nline3\"",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "str", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"line1\nline2\nline3\"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "line1\nline2\nline3"}, Line: 2},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 2},
				},
			},
			{
				// Test for complex nested function calls with whitespace
				source: "outer(  middle  (   inner ( )   )  )",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "outer", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "middle", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "inner", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.LEFT_PAREN, Lexeme: "(", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.RIGHT_PAREN, Lexeme: ")", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for numbers with multiple leading zeros
				source: "num: 000123.000456",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "num", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "000123.000456", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 123.000456}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for mixed operators and whitespace
				source: "result: a +\t-\n* / b",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "result", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "a", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.PLUS, Lexeme: "+", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.MINUS, Lexeme: "-", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STAR, Lexeme: "*", Literal: token.Literal{IsNull: true}, Line: 1},
					{TokenType: token.SLASH, Lexeme: "/", Literal: token.Literal{IsNull: true}, Line: 1},
					{TokenType: token.IDENTIFIER, Lexeme: "b", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 1},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 1},
				},
			},
			{
				// Test for class method with unicode name
				source:   "class Test { método() { return 42 } }",
				expected: nil,
				err:      fmt.Errorf("unexpected é found at line 0"),
			},
			{
				// Test for string with mixed quotes and spaces
				source: "str: \"   'quoted'   \"",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "str", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.STRING, Lexeme: "\"   'quoted'   \"", Literal: token.Literal{Type: token.STRING_LITERAL, StrVal: "   'quoted'   "}, Line: 0},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 0},
				},
			},
			{
				// Test for function with unicode parameters
				source:   "fn sum(π, σ) { return π + σ }",
				expected: nil,
				err:      fmt.Errorf("unexpected π found at line 0"),
			},
			{
				// Test for empty lines with whitespace and comments
				source: "x: 1\n   \n  #comment  \n\t\ny: 2",
				expected: []token.Token{
					{TokenType: token.IDENTIFIER, Lexeme: "x", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 0},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 0},
					{TokenType: token.NUMERIC, Lexeme: "1", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 1}, Line: 0},
					{TokenType: token.IDENTIFIER, Lexeme: "y", Literal: token.Literal{Type: token.STRING_LITERAL, IsNull: true}, Line: 4},
					{TokenType: token.COLON, Lexeme: ":", Literal: token.Literal{IsNull: true}, Line: 4},
					{TokenType: token.NUMERIC, Lexeme: "2", Literal: token.Literal{Type: token.NUMERIC_LITERAL, DoubleVal: 2}, Line: 4},
					{TokenType: token.EOF, Lexeme: "", Literal: token.Literal{IsNull: true}, Line: 4},
				},
			},
		}

		for _, test := range tests {
			actual, err := Lexer(test.source)
			if err != nil {
				assert.EqualError(t, err, fmt.Sprint(test.err))
			}
			assert.Equal(t, test.expected, actual)

		}
	})
}

func TestPeek(t *testing.T) {
	type testCase struct {
		source string
		curr   int

		expected rune
	}

	t.Run("Peek", func(t *testing.T) {
		tests := []testCase{
			{
				source:   "hello",
				curr:     0,
				expected: 'e', // Peek should return next character
			},
			{
				source:   "hello",
				curr:     4,
				expected: 0, // At end of string
			},
			{
				source:   "hello",
				curr:     5,
				expected: 0, // Beyond string length
			},
			{
				source:   "你好", // Chinese characters
				curr:     0,
				expected: '好', // Peek next Chinese character
			},
			{
				source:   "你好",
				curr:     1,              // Middle of UTF-8 bytes
				expected: utf8.RuneError, // Invalid position
			},
			{
				source:   "🌍world", // Emoji (4 bytes) + ascii
				curr:     0,
				expected: 'w', // Peek should return 'w' after emoji
			},
			{
				source:   "🌍world",
				curr:     utf8.RuneError, // Middle of emoji bytes
				expected: 0,              // Invalid position
			},
			{
				source:   "π = 3.14",
				curr:     0,
				expected: ' ', // Peek space after π
			},
			{
				source:   "",
				curr:     0,
				expected: 0, // Empty string
			},
			{
				source:   "hello世界",
				curr:     4,
				expected: '世', // Peek Chinese character after ASCII
			},
		}

		for i, test := range tests {
			actual := peek(test.source, test.curr)
			assert.Equal(t, test.expected, actual, fmt.Sprintf("ASSERT EQUAL ERROR on Test Case #%v: Expected %v does not match with actual %v on source = %v and curr = %v", i, test.expected, actual, test.source, test.curr))

		}
	})
}

func TestAddToken(t *testing.T) {
	type testCase struct {
		tokens    []token.Token
		tokenType token.TokenType
		lexeme    string
		literal   token.Literal
		line      int

		expected []token.Token
	}

	t.Run("AddToken", func(t *testing.T) {
		tests := []testCase{
			{
				// Add numeric token to empty list
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
				// Add operator after numeric
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
				// Add string token
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
				// Add keyword token
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
				// Add token with different line number
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
				// Add boolean token
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
				// Add identifier token
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
				// Add empty string token
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
				// Add function keyword token
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
				// Add comparison operator token
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
			addToken(&test.tokens, test.tokenType, test.lexeme, test.literal, test.line)
			assert.Equal(t, test.expected, test.tokens)
		}
	})
}

func TestGetLexeme(t *testing.T) {
	type testCase struct {
		source string
		start  int
		curr   int

		expected string
	}

	t.Run("GetLexeme", func(t *testing.T) {
		tests := []testCase{
			{
				source:   "hello",
				start:    0,
				curr:     1,
				expected: "h",
			},
			{
				source:   "hello",
				start:    0,
				curr:     5,
				expected: "hello",
			},
			{
				source:   "你好",
				start:    0,
				curr:     3, // First Chinese character is 3 bytes
				expected: "你",
			},
			{
				source:   "🌍world",
				start:    0,
				curr:     4, // Emoji is 4 bytes
				expected: "🌍",
			},
		}

		for _, test := range tests {
			actual := getLexeme(test.source, test.start, test.curr)
			assert.Equal(t, test.expected, actual)
		}
	})
}

func TestIsAtEnd(t *testing.T) {
	type testCase struct {
		source string
		curr   int

		expected bool
	}

	t.Run("IsAtEnd", func(t *testing.T) {
		tests := []testCase{
			{
				source:   "hello",
				curr:     0,
				expected: false,
			},
			{
				source:   "hello",
				curr:     5,
				expected: true,
			},
			{
				source:   "",
				curr:     0,
				expected: true,
			},
			{
				source:   "你好", // Two Chinese characters
				curr:     2,
				expected: true,
			},
			{
				source:   "🌍world", // Emoji + ASCII
				curr:     6,
				expected: true,
			},
		}

		for _, test := range tests {
			actual := isAtEnd(test.source, test.curr)
			assert.Equal(t, test.expected, actual, fmt.Sprintf("source: %q, curr: %d", test.source, test.curr))
		}
	})
}
