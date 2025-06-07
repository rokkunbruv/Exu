package token

import (
	"errors"
	"fmt"
)

type literalType int

const (
	STRING_LITERAL literalType = iota
	NUMERIC_LITERAL
)

// A literal data type that can store EITHER string values or numeric values
// A literal can be null if IsNull = true
type Literal struct {
	StrVal    string
	DoubleVal float64
	Type      literalType
	IsNull    bool
}

func (l *Literal) toString() (string, error) {
	switch {
	case l.Type == STRING_LITERAL:
		return l.StrVal, nil
	case l.Type == NUMERIC_LITERAL:
		return fmt.Sprint(l.DoubleVal), nil
	case l.IsNull:
		return "null", nil
	default:
		return "", errors.New("invalid literal")
	}
}

type Token struct {
	TokenType TokenType
	Lexeme    string
	Literal   Literal
	Line      int
}

func (t *Token) ToString() (string, error) {
	var literal, error = t.Literal.toString()
	if error != nil {
		return "", error
	}
	return string(t.TokenType) + " " + t.Lexeme + " " + literal, nil
}
