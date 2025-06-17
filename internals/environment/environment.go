// Keeps track of the current states (variable, value pairs) of a scope

package environment

import (
	"fmt"

	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
)

type Environment struct {
	values    map[string]literal.Literal // Current states
	enclosing *Environment               // The scope the current environment is enclosed in
	// By default, the environment is enclosed in a global scope (enclosing = nil)
}

func GenerateEnvironment(enclosing *Environment) Environment {
	env := Environment{
		values:    map[string]literal.Literal{},
		enclosing: enclosing,
	}

	return env
}

// Add a new state
func (e *Environment) Define(name string, value literal.Literal) {
	e.values[name] = value
}

// Gets the value of a state from the current scope
func (e *Environment) Get(name token.Token) (literal.Literal, error) {
	value, ok := e.values[name.Lexeme]
	if ok {
		return value, nil
	}

	// Check outer scopes if the variable is defined there
	if e.enclosing != nil {
		return e.enclosing.Get(name)
	}

	// Return error if the variable isnt defined in literal.Literal of the scopes
	return nil, &exu_err.RuntimeError{
		Token:   name,
		Message: fmt.Sprintf("Cannot get value of undefined variable \"%v\"", name.Lexeme),
	}
}

// Performs assignment to an already existing state
func (e *Environment) Assign(name token.Token, value literal.Literal) error {
	_, ok := e.values[name.Lexeme]
	if ok {
		e.values[name.Lexeme] = value
		return nil
	}

	// Check outer scopes if the variable is defined there
	if e.enclosing != nil {
		return e.enclosing.Assign(name, value)
	}

	// Return error if the variable isnt defined in literal.Literal of the scopes
	return &exu_err.RuntimeError{
		Token:   name,
		Message: fmt.Sprintf("Cannot assign to undefined variable \"%v\"", name.Lexeme),
	}
}
