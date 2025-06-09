package token

import (
	"fmt"
)

type literalType int

const (
	STRING_LITERAL literalType = iota
	NUMERIC_LITERAL
)

// A literal data type that can store EITHER string values or numeric values.
// A literal can be null if IsNull = true.
type Literal struct {
	StrVal    string      // If Type = STRING_LITERAL, stores the value of the string literal
	DoubleVal float64     // If Type = NUMERIC_LITERAL, stores the value of the numeric literal
	Type      literalType // Either STRING_LITERAL or NUMERIC_LITERAL
	IsNull    bool        // If true, considers the literal as null and StrVal and DoubleVal will be ignored
}

func (l *Literal) ToString() (string, error) {
	literalVal, err := l.GetVal()
	if err != nil {
		return "", err
	}
	// Return "null" instead of "<nil>"
	if literalVal == nil {
		return "null", nil
	}
	return fmt.Sprint(literalVal), nil
}

// Getter for literal
// When accessing the value of a literal, a type check will be performed
// to ensure that the correct value will be extracted.
// E.g. It doesn't make sense for check for DoubleVal for a string literal,
// hence we need to check if Type == STRING_LITERAL
func (l *Literal) GetVal() (any, error) {
	switch {
	// First check if the literal is null to properly enforce its "null value"
	// despite containing non-null StrVal or DoubleVal
	case l.IsNull:
		return nil, nil
	case l.Type == STRING_LITERAL:
		return l.StrVal, nil
	case l.Type == NUMERIC_LITERAL:
		return l.DoubleVal, nil
	default:
		return nil, fmt.Errorf("unexpected error when getting the value of a literal. check if the literal type is valid")
	}
}

func (l *Literal) SetVal(newVal any) error {
	switch {
	case !l.IsNull && newVal == nil:
		l.IsNull = true
	case l.Type == STRING_LITERAL:
		// Extract the value as string
		s, ok := newVal.(string)
		if !ok {
			return fmt.Errorf("invalid value set to a string literal")
		}
		l.StrVal = s
		if l.IsNull {
			l.IsNull = false
		}
	case l.Type == NUMERIC_LITERAL:
		// Extract the value as float64
		n, ok := newVal.(float64)
		if !ok {
			return fmt.Errorf("invalid value set to a numeric literal")
		}
		l.DoubleVal = n
		if l.IsNull {
			l.IsNull = false
		}
	default:
		return fmt.Errorf("unexpected error when setting the value of a literal. check if the literal type is valid")
	}
	return nil
}

type Token struct {
	TokenType TokenType // Type of token defined in tokenType.go
	Lexeme    string    // Token as it is found in the source
	Literal   Literal   // Value if a literal
	Line      int       // At which line the token is found
}

func (t *Token) ToString() (string, error) {
	var literal, err = t.Literal.ToString()
	if err != nil {
		return "", err
	}
	// Return empty string if the token is null
	if string(t.TokenType) == "" && t.Lexeme == "" && literal == "" {
		return "", nil
	}
	return string(t.TokenType) + " " + t.Lexeme + " " + literal, nil
}
