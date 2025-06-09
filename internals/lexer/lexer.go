package lexer

import (
	"fmt"
	"strconv"
	"unicode/utf8"

	"github.com/rokkunbruv/internals/token"
)

// Converts source code into a slice of tokens
func Lexer(source string) ([]token.Token, error) {
	var tokens []token.Token

	var keywords = map[string](token.TokenType){
		"true":   token.TRUE,
		"false":  token.FALSE,
		"null":   token.NULL,
		"if":     token.IF,
		"else":   token.ELSE,
		"for":    token.FOR,
		"while":  token.WHILE,
		"fn":     token.FN,
		"return": token.RETURN,
		"print":  token.PRINT,
		"class":  token.CLASS,
		"super":  token.SUPER,
		"self":   token.SUPER,
	}

	// Pointers for iterating over the source code
	start, curr, line := 0, 0, 0

	// Scan tokens
	for curr <= len(source) {
		if isAtEnd(source, curr) {
			break
		}

		// Immediately throws an error before checking the character
		// if the curr pointer is on an invalid index

		// The invalid index means that the curr pointer is in the middle
		// of a Unicode character byte sequence

		// This also prevents this error handling in between the mess
		// that is the switch statement below

		// Note that this same code is also present on some cases of the
		// switch statement since in those cases, it does not go back
		// to the start of the loop on the next iteration, which
		// are typically cases for comments of multicharacter tokens
		if peek(source, curr) == utf8.RuneError {
			return nil, fmt.Errorf("UTF-8 decoding error at line %v", line)
		}

		// Sets the start of the token string to the curr pointer location
		start = curr

		// Get current character
		c, n := utf8.DecodeRuneInString(source[curr:])

		// Setting the end bounds on getLexeme at curr+n
		// ensures that the last character of the lexeme is scanned
		switch c {
		case '(':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.LEFT_PAREN, lexeme, literal, line)
		case ')':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.RIGHT_PAREN, lexeme, literal, line)
		case '{':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.LEFT_BRACE, lexeme, literal, line)
		case '}':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.RIGHT_BRACE, lexeme, literal, line)
		case ',':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.COMMA, lexeme, literal, line)
		case '.':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.DOT, lexeme, literal, line)
		case '-':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.MINUS, lexeme, literal, line)
		case '+':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.PLUS, lexeme, literal, line)
		case ';':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.SEMICOLON, lexeme, literal, line)
		case '*':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.STAR, lexeme, literal, line)
		case '/':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.SLASH, lexeme, literal, line)
		case '=':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.EQUAL, lexeme, literal, line)
		case ':':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.COLON, lexeme, literal, line)
		case '#':
			// Skip rest of the line if it is a comment (preceded by #)
			for peek(source, curr) != '\n' && !isAtEnd(source, curr) {
				if peek(source, curr) == utf8.RuneError {
					return nil, fmt.Errorf("UTF-8 decoding error at line %v", line)
				}

				curr += n
				c, n = utf8.DecodeRuneInString(source[curr:])
			}
		case '&':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.AND, lexeme, literal, line)
		case '|':
			literal := token.Literal{IsNull: true}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, token.OR, lexeme, literal, line)
		case '!':
			literal := token.Literal{IsNull: true}
			var tokenType token.TokenType
			if peek(source, curr) == '=' {
				tokenType = token.NOT_EQUAL

				// Consume the next character
				curr += n
				c, n = utf8.DecodeRuneInString(source[curr:])
			} else if peek(source, curr) == utf8.RuneError {
				return nil, fmt.Errorf("UTF-8 decoding error at line %v", line)
			} else {
				tokenType = token.NOT
			}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, tokenType, lexeme, literal, line)
		case '<':
			literal := token.Literal{IsNull: true}
			var tokenType token.TokenType
			if peek(source, curr) == '=' {
				tokenType = token.LESS_EQUAL

				// Consume the next character
				curr += n
				c, n = utf8.DecodeRuneInString(source[curr:])
			} else if peek(source, curr) == '-' {
				tokenType = token.LEFT_ARROW

				// consume the next character
				curr += n
				c, n = utf8.DecodeRuneInString(source[curr:])
			} else if peek(source, curr) == utf8.RuneError {
				return nil, fmt.Errorf("UTF-8 decoding error at line %v", line)
			} else {
				tokenType = token.LESS
			}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, tokenType, lexeme, literal, line)
		case '>':
			literal := token.Literal{IsNull: true}
			var tokenType token.TokenType
			if peek(source, curr) == '=' {
				tokenType = token.GREATER_EQUAL

				// Consume the next character
				curr += n
				c, n = utf8.DecodeRuneInString(source[curr:])
			} else if peek(source, curr) == utf8.RuneError {
				return nil, fmt.Errorf("UTF-8 decoding error at line %v", line)
			} else {
				tokenType = token.GREATER
			}
			lexeme := getLexeme(source, start, curr+n)
			addToken(&tokens, tokenType, lexeme, literal, line)

		// This does nothing, equivalent to adding a break statement to this
		// case except this break is automatically added in Go
		case ' ', '\r', '\t':

		case '\n':
			line++

		// Scans for a potential string
		case '"':
			// Keeps track of the size of the previous character
			// Necessary when we want to access the previous character
			// Updated as curr moves to the next character
			prevN := n

			// Consume the opening "
			curr += n
			c, n = utf8.DecodeRuneInString(source[curr:])

			for c != '"' && !isAtEnd(source, curr) {
				if c == '\n' {
					line++
				}
				if peek(source, curr) == utf8.RuneError {
					return nil, fmt.Errorf("UTF-8 decoding error at line %v", line)
				}

				prevN = n
				curr += n
				c, n = utf8.DecodeRuneInString(source[curr:])
			}

			if isAtEnd(source, curr) {
				return nil, fmt.Errorf("unterminated string at line %v", line)
			}

			// Consume the closing "
			prevN = n
			curr += n
			c, n = utf8.DecodeRuneInString(source[curr:])

			// Get size of the character next to start
			_, nxtS := utf8.DecodeRuneInString(source[start:])

			lexeme := getLexeme(source, start, curr)
			// Trims the quotation mark characters
			// start+nxtS is equivalent to start+1 for an ASCII source string
			// curr-prevN is equivalent to curr-1 for an ASCII source string
			var value string = source[start+nxtS : curr-prevN]
			literal := token.Literal{Type: token.STRING_LITERAL}
			literal.SetVal(value)
			addToken(&tokens, token.STRING, lexeme, literal, line)

			// Since we are on the character after the second quotation mark
			// We skip the curr to next character logic below
			// To start the iteration on this character
			continue

		default:
			switch {
			// Scans for potential numerics
			case isDigit(c):
				// Consume the decimal part of the numeric
				for isDigit(c) {
					if peek(source, curr) == utf8.RuneError {
						return nil, fmt.Errorf("UTF-8 decoding error at line %v", line)
					}

					curr += n
					c, n = utf8.DecodeRuneInString(source[curr:])
				}

				// Consume the fractional part if it exists
				if c == '.' && isDigit(peek(source, curr)) {
					// Consumes the .
					curr += n
					c, n = utf8.DecodeRuneInString(source[curr:])

					// Consumes the fractional digits of the numeric
					for isDigit(c) {
						curr += n
						c, n = utf8.DecodeRuneInString(source[curr:])
					}
				}

				lexeme := getLexeme(source, start, curr)
				// Convert the string representation of the numeric value
				// To an actual numeric value represented by a
				// 64-bit floating point
				value, err := strconv.ParseFloat(source[start:curr], 64)
				if err != nil {
					return nil, err
				}
				literal := token.Literal{Type: token.NUMERIC_LITERAL}
				literal.SetVal(value)
				addToken(&tokens, token.NUMERIC, lexeme, literal, line)

				// We skip the curr to next character logic
				// To prevent skipping the character next to the numeric
				continue

			// Tokenizes identifiers/keywords
			// For identifiers, it only accept names that does not start with a digit
			case isAlpha(c), c == '_':
				// Consume the first character in the identifier/keyword
				curr += n
				c, n = utf8.DecodeRuneInString(source[curr:])

				for isAlphaNum(c) || c == '_' {
					if peek(source, curr) == utf8.RuneError {
						return nil, fmt.Errorf("UTF-8 decoding error at line %v", line)
					}

					curr += n
					c, n = utf8.DecodeRuneInString(source[curr:])
				}

				lexeme := getLexeme(source, start, curr)

				// Check if the lexeme is in the list of keywords
				// If it exists, assign type to that keyword type
				// If not, set it as an identifier
				tokenType, ok := keywords[lexeme]
				if !ok {
					tokenType = token.IDENTIFIER
				}
				literal := token.Literal{Type: token.STRING_LITERAL, IsNull: true}
				addToken(&tokens, tokenType, lexeme, literal, line)

				// We skip the curr to next character logic
				// To prevent skipping the character next to the identifier/keyword
				continue

			// Error handling for unexpected characters
			default:
				return nil, fmt.Errorf("unexpected %v found at line %v", string(c), line)
			}
		}

		// Move to the next character
		curr += n
	}

	addToken(&tokens, token.EOF, "", token.Literal{IsNull: true}, line)
	return tokens, nil
}

// HELPER FUNCTIONS

// Appends a token to the tokens slice
func addToken(tokens *[]token.Token, tokenType token.TokenType, lexeme string, literal token.Literal, line int) {
	*tokens = append(*tokens, token.Token{TokenType: tokenType, Lexeme: lexeme, Literal: literal, Line: line})
}

// Extracts the token lexeme from the source string
func getLexeme(source string, start int, end int) string {
	return source[start:end]
}

// Checks if the curr character pointer reaches the end of the source line
func isAtEnd(source string, curr int) bool {
	return curr >= utf8.RuneCountInString(source)
}

// Peeks at the next character in the string
// Returns utf8.RuneError to indicate a UTF-8 decoding error when calling peek
func peek(source string, curr int) rune {
	if curr >= len(source) {
		return 0
	}

	// Get the current rune
	_, n := utf8.DecodeRuneInString(source[curr:])

	if curr+n >= len(source) {
		return 0
	}

	// Get the next rune
	r, _ := utf8.DecodeRuneInString(source[curr+n:])

	return r
}

func isDigit(d rune) bool {
	return d >= '0' && d <= '9'
}

func isAlpha(c rune) bool {
	return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
}

func isAlphaNum(c rune) bool {
	return isDigit(c) || isAlpha(c)
}
