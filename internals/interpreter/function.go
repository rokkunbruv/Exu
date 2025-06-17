package interpreter

import (
	"fmt"

	"github.com/rokkunbruv/internals/environment"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/statement"
)

// Special error type to immediately exit a function
type Return struct {
	Value literal.Literal
}

func (*Return) Error() string {
	// Return does not need an error message
	return ""
}

// Function object
type Function struct {
	Declaration statement.Function      // Function definition
	Closure     environment.Environment // "Enclosing" of function environment; The environment where the function is declared
}

func (f *Function) Call(i Interpreter, args []literal.Literal) (literal.Literal, error) {
	// Create own scope for function
	env := environment.GenerateEnvironment(&f.Closure)

	// Define parameters within function scope
	for i, param := range f.Declaration.Params {
		env.Define(param.Lexeme, args[i])
	}

	// Execute function body
	err := i.executeBlock(f.Declaration.Body, env)
	if err != nil {
		// Check if thrown error is of return type
		// If it is of return type, return the value of
		// the return statement
		returnVal, ok := err.(*Return)
		if ok {
			return returnVal.Value, nil
		}

		// Otherwise it is a normal error, hence we throw it
		return nil, err
	}

	// Return a null literal by default after a function call
	return &literal.NullLiteral{}, nil
}

func (f *Function) Arity() int {
	return len(f.Declaration.Params)
}

func (f *Function) ToString() string {
	return fmt.Sprintf("<%v: fn>", f.Declaration.Name.Lexeme)
}

func (*Function) Type() string {
	return "Function"
}
