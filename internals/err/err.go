package exu_err

import (
	"fmt"

	"github.com/rokkunbruv/internals/token"
)

// Invokes on lexing errors (unsupported characters, unterminated strings. utf-8 decoding errors)
type ScanError struct {
	Line    int
	Message string
}

func (e *ScanError) Error() string {
	return fmt.Sprintf("Scan error on line %v: %v", e.Line, e.Message)
}

// Invokes on syntax errors (invalid expressions)
type SyntaxError struct {
	Token   token.Token
	Message string
}

func (e *SyntaxError) Error() string {
	if e.Token.TokenType == token.EOF {
		return fmt.Sprintf("Syntax error on line %v at end: %v", e.Token.Line, e.Message)
	}
	return fmt.Sprintf("Syntax error on line %v: %v", e.Token.Line, e.Message)
}

// Invokes on parsing errors (invalid access to tokens)
type ParseError struct {
	Curr    int
	IsEmpty bool
	Message string
}

func (e *ParseError) Error() string {
	if e.IsEmpty {
		return fmt.Sprintf("Parse error: Empty tokens list")
	}
	return fmt.Sprintf("Parse error on index %v in tokens: %v", e.Curr, e.Message)
}

// Invokes on runtime errors (invalid operation between types)
type RuntimeError struct {
	Token   token.Token
	Message string
}

func (e *RuntimeError) Error() string {
	return fmt.Sprintf("Runtime error on line %v: %v", e.Token.Line, e.Message)
}

// Invokes on interface cast errors (unexpected type)
type CastError struct {
	Message string
}

func (e *CastError) Error() string {
	return fmt.Sprintf("Cast error: %v", e.Message)
}
