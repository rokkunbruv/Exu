package token

import (
	"github.com/rokkunbruv/internals/literal"
)

type Token struct {
	TokenType TokenType       // Type of token defined in tokenType.go
	Lexeme    string          // Token as it is found in the source
	Literal   literal.Literal // Value if a literal
	Line      int             // At which line the token is found
}

func (t *Token) ToString() string {
	lit := ""
	if t.Literal != nil {
		lit = t.Literal.ToString()
	}
	// Return empty string if the token is null
	if string(t.TokenType) == "" && t.Lexeme == "" && lit == "" {
		return ""
	}
	return string(t.TokenType) + " " + t.Lexeme + " " + lit
}
