package interpreter

import (
	"testing"

	"github.com/rokkunbruv/internals/environment"
	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expression"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/statement"
	"github.com/rokkunbruv/internals/token"
	"github.com/stretchr/testify/assert"
)

// Primarily tests return values on function call
func TestCall(t *testing.T) {
	type testCase struct {
		name string

		function    Function
		interpreter Interpreter
		args        []literal.Literal

		expected literal.Literal
		err      error
	}

	tests := []testCase{
		{
			name: "test fn w/ no return value",
			function: Function{
				Declaration: statement.Function{
					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{},
					Body: []statement.Stmt{
						&statement.Expression{
							Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						},
					},
				},
				Closure: environment.GenerateEnvironment(nil),
			},
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			args:     []literal.Literal{},
			expected: &literal.NullLiteral{},
		},
		{
			name: "test fn w/ return value",
			function: Function{
				Declaration: statement.Function{
					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{},
					Body: []statement.Stmt{
						&statement.Return{
							Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						},
					},
				},
				Closure: environment.GenerateEnvironment(nil),
			},
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			args:     []literal.Literal{},
			expected: literal.GenerateNumericLiteral(1),
		},
		{
			name: "test fn w/ expr return value",
			function: Function{
				Declaration: statement.Function{
					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{},
					Body: []statement.Stmt{
						&statement.Return{
							Value: &expression.Binary{
								Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
								Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							},
						},
					},
				},
				Closure: environment.GenerateEnvironment(nil),
			},
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			args:     []literal.Literal{},
			expected: literal.GenerateNumericLiteral(2),
		},
		{
			name: "test fn w/ just return",
			function: Function{
				Declaration: statement.Function{
					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{},
					Body: []statement.Stmt{
						&statement.Return{Value: nil},
					},
				},
				Closure: environment.GenerateEnvironment(nil),
			},
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			args:     []literal.Literal{},
			expected: &literal.NullLiteral{},
		},
		{
			name: "test fn returning its arg",
			function: Function{
				Declaration: statement.Function{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{
						{TokenType: token.IDENTIFIER, Lexeme: "b"},
					},
					Body: []statement.Stmt{
						&statement.Return{
							Value: &expression.Variable{
								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
							},
						},
					},
				},
				Closure: environment.GenerateEnvironment(nil),
			},
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			args:     []literal.Literal{literal.GenerateNumericLiteral(1)},
			expected: literal.GenerateNumericLiteral(1),
		},
		{
			name: "test fn w/ invalid body",
			function: Function{
				Declaration: statement.Function{
					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{},
					Body: []statement.Stmt{
						&statement.Expression{
							Expression: &expression.Binary{
								Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
								Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
								Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
							},
						},
					},
				},
				Closure: environment.GenerateEnvironment(nil),
			},
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			args: []literal.Literal{},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				Message: "Operation + cannot be performed on type Bool and type Bool",
			},
		},
		// {
		// 	name: "test recursive fn",
		// 	function: Function{
		// 		Declaration: statement.Function{
		// 			Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
		// 			Params: []token.Token{
		// 				{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 			},
		// 			Body: []statement.Stmt{
		// 				&statement.If{
		// 					Condition: &expression.Binary{
		// 						Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"}},
		// 						Operator: token.Token{TokenType: token.LESS, Lexeme: "<"},
		// 						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(0)},
		// 					},
		// 					ThenBranch: &statement.Return{
		// 						Value: &expression.Literal{Value: literal.GenerateNumericLiteral(0)},
		// 					},
		// 				},
		// 				&statement.Return{
		// 					Value: &expression.Binary{
		// 						Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"}},
		// 						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
		// 						Right: &expression.Call{
		// 							Callee: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
		// 							Paren:  token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
		// 							Arguments: []expression.Expr{
		// 								&expression.Binary{
		// 									Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"}},
		// 									Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
		// 									Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
		// 								},
		// 							},
		// 						},
		// 					},
		// 				},
		// 			},
		// 		},
		// 		Closure: UseNativeFunctions(),
		// 	},
		// 	interpreter: Interpreter{
		// 		Globals: UseNativeFunctions(),
		// 		env:     UseNativeFunctions(),
		// 	},
		// 	args:     []literal.Literal{literal.GenerateNumericLiteral(3)},
		// 	expected: literal.GenerateNumericLiteral(6),
		// },
		{
			name:     "test fn w/o declaration",
			function: Function{},
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			args:     []literal.Literal{},
			expected: &literal.NullLiteral{},
		},
		{
			name: "test fn w/o closure",
			function: Function{
				Declaration: statement.Function{
					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{},
					Body: []statement.Stmt{
						&statement.Return{Value: nil},
					},
				},
			},
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			args:     []literal.Literal{},
			expected: &literal.NullLiteral{},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := test.function.Call(test.interpreter, test.args)

			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestArity(t *testing.T) {
	type testCase struct {
		name string

		function Function

		expected int
	}

	tests := []testCase{
		{
			name: "test function w/ one param",
			function: Function{
				Declaration: statement.Function{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{
						{TokenType: token.IDENTIFIER, Lexeme: "b"},
					},
					Body: []statement.Stmt{},
				},
				Closure: UseNativeFunctions(),
			},
			expected: 1,
		},
		{
			name: "test function w/ two param",
			function: Function{
				Declaration: statement.Function{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{
						{TokenType: token.IDENTIFIER, Lexeme: "b"},
						{TokenType: token.IDENTIFIER, Lexeme: "b"},
					},
					Body: []statement.Stmt{},
				},
				Closure: UseNativeFunctions(),
			},
			expected: 2,
		},
		{
			name: "test function w/ no params",
			function: Function{
				Declaration: statement.Function{
					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Params: []token.Token{},
					Body:   []statement.Stmt{},
				},
				Closure: UseNativeFunctions(),
			},
			expected: 0,
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual := test.function.Arity()
			assert.Equal(t, test.expected, actual)
		})
	}
}
