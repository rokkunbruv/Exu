// Native function definitions
package interpreter

import (
	"time"

	"github.com/rokkunbruv/internals/environment"
	"github.com/rokkunbruv/internals/literal"
)

// Package native functions to in an environment to be usable by the programmer
func UseNativeFunctions() environment.Environment {
	globals := environment.GenerateEnvironment(nil)
	globals.Define("clock", &Clock{})

	return globals
}

// Returns current Unix time in seconds
type Clock struct{}

func (c *Clock) Arity() int {
	return 0
}

func (c *Clock) Call(i Interpreter, args []literal.Literal) (literal.Literal, error) {
	// Cast Unix time to float64 to create new numeric literal
	return literal.GenerateNumericLiteral(float64(time.Now().Unix())), nil
}

func (*Clock) ToString() string {
	return "<clock: native>"
}

func (*Clock) Type() string {
	return "Native Function"
}
