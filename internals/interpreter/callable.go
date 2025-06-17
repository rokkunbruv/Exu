package interpreter

import "github.com/rokkunbruv/internals/literal"

// Can be attached to any object that's callable (functions & class methods)
type Callable interface {
	literal.Literal

	Call(interpreter Interpreter, arguments []literal.Literal) (literal.Literal, error)
	Arity() int
}
