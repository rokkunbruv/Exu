// This test checks for either of the two behaviors: output stream behavior or interpreter state behavior
// Output stream test checks for command line outputs, used to test print statements, scoping, and control flow
// Interpreter state test checks for interpreter state, used for test initialization of objects and variables

package interpreter

import (
	"bytes"
	"testing"

	"github.com/rokkunbruv/internals/environment"
	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expression"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/statement"
	"github.com/rokkunbruv/internals/token"
	"github.com/stretchr/testify/assert"
)

func TestExecuteOnInterpreterBehavior(t *testing.T) {
	type testCase struct {
		name        string
		interpreter Interpreter

		statement statement.Stmt

		expected Interpreter
		err      error
	}

	tests := []testCase{
		{
			name: "test initialize a without initializer",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			statement: &statement.Let{
				Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", &literal.NullLiteral{})
					return env
				}(),
			},
		},
		{
			name: "test define a to be 1",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			statement: &statement.Let{
				Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
		},
		{
			name: "test define a to be \"str\" + \"str\"",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			statement: &statement.Let{
				Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Initializer: &expression.Binary{
					Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
					Right:    &expression.Literal{Value: literal.GenerateStringLiteral("str")},
				},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", literal.GenerateStringLiteral("strstr"))
					return env
				}(),
			},
		},
		{
			name: "test define a to be !\"str\"",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			statement: &statement.Let{
				Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Initializer: &expression.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateStringLiteral("str")},
				},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.STAR, Lexeme: "!", Line: 1},
				Message: "Operation ! cannot be performed on type String",
			},
		},
		{
			name: "test redefine a, previously a = nil, to be 1",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", &literal.NullLiteral{})
					return env
				}(),
			},
			statement: &statement.Let{
				Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
		},
		{
			name: "test redefine a, previously a = 1, to be \"str\"",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			statement: &statement.Let{
				Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Initializer: &expression.Literal{Value: literal.GenerateStringLiteral("str")},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", literal.GenerateStringLiteral("str"))
					return env
				}(),
			},
		},
		{
			name: "test redefine a, previously a = 1, to be !1",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			statement: &statement.Let{
				Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Initializer: &expression.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "test expression statement",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			statement: &statement.Expression{
				Expression: &expression.Binary{
					Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
		},
		{
			name: "test function declaration",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			statement: &statement.Function{
				Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Params: []token.Token{},
				Body: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					},
				},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{},
							Body: []statement.Stmt{
								&statement.Expression{
									Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								},
							},
						},
						Closure: env,
					})
					return env
				}(),
			},
		},
		{
			name: "test local function declaration",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			statement: &statement.Function{
				Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Params: []token.Token{},
				Body: []statement.Stmt{
					&statement.Function{
						Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
						Params: []token.Token{},
						Body: []statement.Stmt{
							&statement.Expression{
								Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							},
						},
					},
				},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{},
							Body: []statement.Stmt{
								&statement.Function{
									Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
									Params: []token.Token{},
									Body: []statement.Stmt{
										&statement.Expression{
											Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
										},
									},
								},
							},
						},
						Closure: env,
					})
					return env
				}(),
			},
		},
		{
			name: "test function declaration w/ invalid body",
			interpreter: Interpreter{
				Globals: UseNativeFunctions(),
				env:     UseNativeFunctions(),
			},
			statement: &statement.Function{
				Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				Params: []token.Token{},
				Body: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Binary{
							Left: &expression.Literal{
								Value: literal.GenerateBoolLiteral(true),
							},
							Operator: token.Token{
								TokenType: token.PLUS,
								Lexeme:    "+",
								Literal:   nil,
								Line:      1,
							},
							Right: &expression.Literal{
								Value: literal.GenerateBoolLiteral(true),
							},
						},
					},
				},
			},
			expected: Interpreter{
				Globals: UseNativeFunctions(),
				env: func() environment.Environment {
					env := UseNativeFunctions()
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{},
							Body: []statement.Stmt{
								&statement.Expression{
									Expression: &expression.Binary{
										Left: &expression.Literal{
											Value: literal.GenerateBoolLiteral(true),
										},
										Operator: token.Token{
											TokenType: token.PLUS,
											Lexeme:    "+",
											Literal:   nil,
											Line:      1,
										},
										Right: &expression.Literal{
											Value: literal.GenerateBoolLiteral(true),
										},
									},
								},
							},
						},
						Closure: env,
					})
					return env
				}(),
			},
		},
		// FIXME: Make this work
		// {
		// 	name: "test declaring variable initialized to function call returning function",
		// 	interpreter: Interpreter{
		// 		Globals: UseNativeFunctions(),
		// 		env: func() environment.Environment {
		// 			env := UseNativeFunctions()
		// 			closure := environment.GenerateEnvironment(nil)
		// 			closure.Define("b", &Function{
		// 				Declaration: statement.Function{
		// 					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 					Params: []token.Token{},
		// 					Body:   []statement.Stmt{},
		// 				},
		// 			})
		// 			env.Define("a", &Function{
		// 				Declaration: statement.Function{
		// 					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
		// 					Params: []token.Token{},
		// 					Body: []statement.Stmt{
		// 						&statement.Function{
		// 							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 							Params: []token.Token{},
		// 							Body:   []statement.Stmt{},
		// 						},
		// 						&statement.Return{
		// 							Value: &expression.Variable{
		// 								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 							},
		// 						},
		// 					},
		// 				},
		// 				Closure: closure,
		// 			})
		// 			return env
		// 		}(),
		// 	},
		// 	statement: &statement.Let{
		// 		Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "c"},
		// 		Initializer: &expression.Call{
		// 			Callee:    &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
		// 			Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
		// 			Arguments: []expression.Expr{},
		// 		},
		// 	},
		// 	expected: Interpreter{
		// 		Globals: UseNativeFunctions(),
		// 		env: func() environment.Environment {
		// 			env := UseNativeFunctions()
		// 			closure := environment.GenerateEnvironment(nil)
		// 			env.Define("a", &Function{
		// 				Declaration: statement.Function{
		// 					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
		// 					Params: []token.Token{},
		// 					Body: []statement.Stmt{
		// 						&statement.Function{
		// 							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 							Params: []token.Token{},
		// 							Body:   []statement.Stmt{},
		// 						},
		// 						&statement.Return{
		// 							Value: &expression.Variable{
		// 								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 							},
		// 						},
		// 					},
		// 				},
		// 				Closure: closure,
		// 			})
		// 			// The returned function should be in the same environment
		// 			env.Define("c", &Function{
		// 				Declaration: statement.Function{
		// 					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 					Params: []token.Token{},
		// 					Body:   []statement.Stmt{},
		// 				},
		// 				Closure: closure,
		// 			})
		// 			return env
		// 		}(),
		// 	},
		// },
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			err := test.interpreter.execute(test.statement)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}

			assert.Equal(t, test.expected, test.interpreter)
		})
	}
}

func TestExecuteOnOutputStream(t *testing.T) {
	type testCase struct {
		name        string
		interpreter Interpreter

		statement statement.Stmt

		expected string
		err      error
	}

	tests := []testCase{
		// FIXME: Make this work
		// {
		// 	name: "test recursive fn",
		// 	interpreter: Interpreter{
		// 		env: func() environment.Environment {
		// 			env := environment.GenerateEnvironment(nil)
		// 			env.Define("a", &Function{
		// 				Declaration: statement.Function{
		// 					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
		// 					Params: []token.Token{{TokenType: token.IDENTIFIER, Lexeme: "n"}},
		// 					Body: []statement.Stmt{
		// 						&statement.If{
		// 							Condition: &expression.Binary{
		// 								Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "n"}},
		// 								Operator: token.Token{TokenType: token.LESS, Lexeme: "<"},
		// 								Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(0)},
		// 							},
		// 							ThenBranch: &statement.Return{
		// 								Value: &expression.Literal{Value: literal.GenerateNumericLiteral(0)},
		// 							},
		// 						},
		// 						&statement.Return{
		// 							Value: &expression.Binary{
		// 								Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "n"}},
		// 								Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
		// 								Right: &expression.Call{
		// 									Callee: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
		// 									Paren:  token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
		// 									Arguments: []expression.Expr{
		// 										&expression.Binary{
		// 											Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "n"}},
		// 											Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
		// 											Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
		// 										},
		// 									},
		// 								},
		// 							},
		// 						},
		// 					},
		// 				},
		// 				Closure: environment.GenerateEnvironment(nil),
		// 			})
		// 			return env
		// 		}(),
		// 	},
		// 	statement: &statement.Print{
		// 		Expression: &expression.Call{
		// 			Callee: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
		// 			Paren:  token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
		// 			Arguments: []expression.Expr{
		// 				&expression.Literal{Value: literal.GenerateNumericLiteral(3)},
		// 			},
		// 		},
		// 	},
		// 	expected: "6\n",
		// },
		{
			name: "test while loop counting from 1 to 5",
			interpreter: Interpreter{
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			statement: &statement.While{
				Condition: &expression.Binary{
					Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
					Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<="},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(5)},
				},
				Body: &statement.Block{
					Statements: []statement.Stmt{
						&statement.Print{
							Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
						},
						&statement.Expression{
							Expression: &expression.Assignment{
								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
								Value: &expression.Binary{
									Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
									Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
									Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								},
							},
						},
					},
				},
			},
			expected: "1\n2\n3\n4\n5\n",
		},
		{
			name: "test while loop w/ nonbool condition",
			interpreter: Interpreter{
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			statement: &statement.While{
				Condition: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
				Body: &statement.Block{
					Statements: []statement.Stmt{
						&statement.Print{
							Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
						},
						&statement.Expression{
							Expression: &expression.Assignment{
								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
								Value: &expression.Binary{
									Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
									Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
									Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								},
							},
						},
					},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{},
				Message: "Condition of while loop does not evaluate to a bool",
			},
		},
		{
			name: "test while loop w/ invalid condition",
			interpreter: Interpreter{
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			statement: &statement.While{
				Condition: &expression.Unary{
					Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
					Right:    &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
				},
				Body: &statement.Block{
					Statements: []statement.Stmt{
						&statement.Print{
							Expression: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
						},
						&statement.Expression{
							Expression: &expression.Assignment{
								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
								Value: &expression.Binary{
									Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
									Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
									Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								},
							},
						},
					},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "test while loop w/ invalid body",
			interpreter: Interpreter{
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			statement: &statement.While{
				Condition: &expression.Binary{
					Left:     &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
					Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<="},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(5)},
				},
				Body: &statement.Block{
					Statements: []statement.Stmt{
						&statement.Expression{
							Expression: &expression.Unary{
								Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
								Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							},
						},
					},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "test executing then stmt",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				ThenBranch: &statement.Print{
					Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
				ElseBranch: &statement.Print{
					Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: "1\n",
		},
		{
			name: "test executing else stmt",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
				ThenBranch: &statement.Print{
					Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
				ElseBranch: &statement.Print{
					Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			expected: "2\n",
		},
		{
			name: "test not executing then stmt",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
				ThenBranch: &statement.Print{
					Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
			},
			expected: "",
		},
		{
			name: "test if w/ nonbool condition",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateStringLiteral("str")},
				ThenBranch: &statement.Print{
					Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
				ElseBranch: &statement.Print{
					Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{},
				Message: "Condition of if statement does not evaluate to a bool",
			},
		},
		{
			name: "test if w/ invalid then stmt",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
				ThenBranch: &statement.Print{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "test if w/ invalid else stmt",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.If{
				Condition: &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
				ThenBranch: &statement.Print{
					Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
				ElseBranch: &statement.Print{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "test print literal",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.Print{
				Expression: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
			},
			expected: "1\n",
		},
		{
			name: "test print expression",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.Print{
				Expression: &expression.Binary{
					Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
					Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
			},
			expected: "2\n",
		},
		{
			name: "test print variable a where a = 1",
			interpreter: Interpreter{
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			statement: &statement.Print{
				Expression: &expression.Variable{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				},
			},
			expected: "1\n",
		},
		{
			name: "test print uninitialized variable a",
			interpreter: Interpreter{
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &literal.NullLiteral{})
					return env
				}(),
			},
			statement: &statement.Print{
				Expression: &expression.Variable{
					Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
				},
			},
			expected: "",
		},
		{
			name: "test print assignment expression",
			interpreter: Interpreter{
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &literal.NullLiteral{})
					return env
				}(),
			},
			statement: &statement.Print{
				Expression: &expression.Assignment{
					Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
					Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
				},
			},
			expected: "1\n",
		},
		{
			name: "test access to local variable a = 1 in its block",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Let{
						Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					},
					&statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						},
					},
				},
			},
			expected: "1\n",
		},
		{
			name: "test access global var a = 1 in a local scope",
			interpreter: Interpreter{
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			statement: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						},
					},
				},
			},
			expected: "1\n",
		},
		{
			name: "test access to global variable a = 1 assigned to 2 in local scope",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Let{
						Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					},
					&statement.Block{
						Statements: []statement.Stmt{
							&statement.Expression{
								Expression: &expression.Assignment{
									Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
									Value: &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
								},
							},
							&statement.Print{
								Expression: &expression.Variable{
									Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
								},
							},
						},
					},
					&statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						},
					},
				},
			},
			expected: "2\n2\n",
		},
		{
			name: "test access to global variable a = 1 and local variable a = 2 in local scope",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Let{
						Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					},
					&statement.Block{
						Statements: []statement.Stmt{
							&statement.Let{
								Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
								Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
							},
							&statement.Print{
								Expression: &expression.Variable{
									Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
								},
							},
						},
					},
					&statement.Print{
						Expression: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						},
					},
				},
			},
			expected: "2\n1\n",
		},
		{
			name: "test access var from a different local scope within its own local scope",
			interpreter: Interpreter{
				env: environment.GenerateEnvironment(nil),
			},
			statement: &statement.Block{
				Statements: []statement.Stmt{
					&statement.Block{
						Statements: []statement.Stmt{
							&statement.Let{
								Name:        token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
								Initializer: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							},
						},
					},
					&statement.Block{
						Statements: []statement.Stmt{
							&statement.Print{
								Expression: &expression.Variable{
									Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
								},
							},
						},
					},
				},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 0},
				Message: "Cannot get value of undefined variable \"a\"",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Output stream
			out = bytes.NewBuffer(nil)

			err := test.interpreter.execute(test.statement)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}

			actual := out.(*bytes.Buffer).String() // Get command line output
			assert.Equal(t, test.expected, actual)
		})
	}
}
