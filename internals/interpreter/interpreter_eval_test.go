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

func TestEvaluate(t *testing.T) {
	type testCase struct {
		name        string
		interpreter Interpreter

		expected literal.Literal
		err      error
	}

	tests := []testCase{
		{
			name: "test fn call returning the result of a local fn call",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee:    &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
							Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
							Arguments: []expression.Expr{},
						},
					},
				},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{},
							Body: []statement.Stmt{
								&statement.Function{
									Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
									Params: []token.Token{},
									Body: []statement.Stmt{
										&statement.Return{
											Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
										},
									},
								},
								&statement.Return{
									Value: &expression.Call{
										Callee:    &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"}},
										Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
										Arguments: []expression.Expr{},
									},
								},
							},
						},
						Closure: environment.GenerateEnvironment(nil),
					})
					return env
				}(),
			},
			expected: literal.GenerateNumericLiteral(1),
		},
		// {
		// 	name: "test fn call returning a function",
		// 	interpreter: Interpreter{
		// 		Statements: []statement.Stmt{
		// 			&statement.Expression{
		// 				Expression: &expression.Call{
		// 					Callee:    &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
		// 					Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
		// 					Arguments: []expression.Expr{},
		// 				},
		// 			},
		// 		},
		// 		env: func() environment.Environment {
		// 			env := environment.GenerateEnvironment(nil)
		// 			env.Define("a", &Function{
		// 				Declaration: statement.Function{
		// 					Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 					Params: []token.Token{},
		// 					Body:   []statement.Stmt{},
		// 				},
		// 				Closure: environment.GenerateEnvironment(nil),
		// 			})
		// 			return env
		// 		}(),
		// 	},
		// 	expected: &Function{
		// 		Declaration: statement.Function{
		// 			Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"},
		// 			Params: []token.Token{},
		// 			Body:   []statement.Stmt{},
		// 		},
		// 		Closure: func() environment.Environment {
		// 			enclosing := environment.GenerateEnvironment(nil)
		// 			env := environment.GenerateEnvironment(&enclosing)
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
		// 							Value: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"}},
		// 						},
		// 					},
		// 				},
		// 				Closure: environment.GenerateEnvironment(nil),
		// 			})
		// 			return env
		// 		}(),
		// 	},
		// },
		{
			name: "test valid function call of already declared fn",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee:    &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
							Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
							Arguments: []expression.Expr{},
						},
					},
				},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{},
							Body:   []statement.Stmt{},
						},
						Closure: environment.GenerateEnvironment(nil),
					})
					return env
				}(),
			},
			expected: &literal.NullLiteral{},
		},
		{
			name: "test valid function call of undeclared fn",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee: &expression.Variable{
								Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
							},
							Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
							Arguments: []expression.Expr{},
						},
					},
				},
				env: environment.GenerateEnvironment(nil),
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Message: "Cannot get value of undefined variable \"a\"",
			},
		},
		{
			name: "test call on non-function variable",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee:    &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
							Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
							Arguments: []expression.Expr{},
						},
					},
				},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Invalid call operation on expression. Only functions and class methods are callable",
			},
		},
		{
			name: "test call on literal",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
							Arguments: []expression.Expr{},
						},
					},
				},
				env: environment.GenerateEnvironment(nil),
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Invalid call operation on expression. Only functions and class methods are callable",
			},
		},
		{
			name: "test valid function call w/ arg",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
							Paren:  token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
							Arguments: []expression.Expr{
								&expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "b"}},
							},
						},
					},
				},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{{TokenType: token.IDENTIFIER, Lexeme: "b"}},
							Body:   []statement.Stmt{},
						},
						Closure: environment.GenerateEnvironment(nil),
					})
					env.Define("b", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			expected: &literal.NullLiteral{},
		},
		{
			name: "test function call w/ mismatched args",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee:    &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
							Paren:     token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
							Arguments: []expression.Expr{},
						},
					},
				},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{{TokenType: token.IDENTIFIER, Lexeme: "b"}},
							Body:   []statement.Stmt{},
						},
						Closure: environment.GenerateEnvironment(nil),
					})
					return env
				}(),
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")", Line: 1},
				Message: "Expected 1 arguments but got 0",
			},
		},
		{
			name: "test function call w/ expression arg",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
							Paren:  token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
							Arguments: []expression.Expr{
								&expression.Binary{
									Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
									Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
									Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								},
							},
						},
					},
				},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{{TokenType: token.IDENTIFIER, Lexeme: "b"}},
							Body:   []statement.Stmt{},
						},
						Closure: environment.GenerateEnvironment(nil),
					})
					return env
				}(),
			},
			expected: &literal.NullLiteral{},
		},
		{
			name: "test function call w/ invalid arg",
			interpreter: Interpreter{
				Statements: []statement.Stmt{
					&statement.Expression{
						Expression: &expression.Call{
							Callee: &expression.Variable{Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"}},
							Paren:  token.Token{TokenType: token.RIGHT_PAREN, Lexeme: ")"},
							Arguments: []expression.Expr{
								&expression.Unary{
									Operator: token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
									Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
								},
							},
						},
					},
				},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &Function{
						Declaration: statement.Function{
							Name:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
							Params: []token.Token{{TokenType: token.IDENTIFIER, Lexeme: "b"}},
							Body:   []statement.Stmt{},
						},
						Closure: environment.GenerateEnvironment(nil),
					})
					return env
				}(),
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!", Line: 1},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "test and resulting to true",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
						Operator: token.Token{TokenType: token.AND, Lexeme: "&"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "test and w/ false 1st expr",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
						Operator: token.Token{TokenType: token.AND, Lexeme: "&"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "test and w/ false 2nd expr",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
						Operator: token.Token{TokenType: token.AND, Lexeme: "&"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "test or resulting to false",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
						Operator: token.Token{TokenType: token.OR, Lexeme: "|"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "test and w/ true 1st expr",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
						Operator: token.Token{TokenType: token.OR, Lexeme: "|"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "test and w/ true 2nd expr",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
						Operator: token.Token{TokenType: token.OR, Lexeme: "|"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "test chained logical expr",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left: &expression.Logical{
							Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
							Operator: token.Token{TokenType: token.AND, Lexeme: "&"},
							Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
						},
						Operator: token.Token{TokenType: token.AND, Lexeme: "&"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "test logical operator precedence",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left: &expression.Logical{
							Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
							Operator: token.Token{TokenType: token.AND, Lexeme: "&"},
							Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
						},
						Operator: token.Token{TokenType: token.OR, Lexeme: "|"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "test logical expression w/ expression that evaluates to bool",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left: &expression.Binary{
							Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
						},
						Operator: token.Token{TokenType: token.AND, Lexeme: "&"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "test logical expression w/ expression that evaluates to non bool",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.AND, Lexeme: "&", Line: 1},
				Message: "Expression before & does not evaluate to a bool",
			},
		},
		{
			name: "test logical expression w/ invalid expression",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Logical{
						Left: &expression.Unary{
							Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						},
						Operator: token.Token{TokenType: token.AND, Lexeme: "&"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!", Line: 0},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "unary not on bool",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "double unary not on bool",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
						Right: &expression.Unary{
							Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
							Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
						}}},
				},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "unary not on expression that evaluates to bool",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
						Right: &expression.Binary{
							Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
						}}},
				},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "unary not on string",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
						Right:    &expression.Literal{Value: literal.GenerateStringLiteral("str")},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!"},
				Message: "Operation ! cannot be performed on type String",
			},
		},
		{
			name: "unary not on numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(0)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!"},
				Message: "Operation ! cannot be performed on type Numeric",
			},
		},
		{
			name: "unary not on null",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.NOT, Lexeme: "!"},
						Right:    &expression.Literal{Value: &literal.NullLiteral{}},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT, Lexeme: "!"},
				Message: "Operation ! cannot be performed on type Null",
			},
		},
		{
			name: "unary minus on numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(23)},
					}}},
			},
			expected: literal.GenerateNumericLiteral(-23),
		},
		{
			name: "double unary minus on numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
						Right: &expression.Unary{
							Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(23)},
						}}},
				},
			},
			expected: literal.GenerateNumericLiteral(23),
		},
		{
			name: "unary minus on expression that evaluates to numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
						Right: &expression.Binary{
							Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
						}}},
				},
			},
			expected: literal.GenerateNumericLiteral(-3),
		},
		{
			name: "unary minus on string",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateStringLiteral("str")},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type String",
			},
		},
		{
			name: "unary minus on bool",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type Bool",
			},
		},
		{
			name: "unary minus on null",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Unary{
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
						Right:    &expression.Literal{Value: &literal.NullLiteral{}},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type Null",
			},
		},

		{
			name: "equal on two expressions that lead to same type",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left: &expression.Binary{
							Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
						},
						Operator: token.Token{TokenType: token.EQUAL, Lexeme: "="},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "equal on two expressions that doesnt lead to same type",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left: &expression.Binary{
							Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
						},
						Operator: token.Token{TokenType: token.EQUAL, Lexeme: "=", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.EQUAL, Lexeme: "=", Line: 0},
				Message: "Operation = cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "not equal on two expressions that lead to same type",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left: &expression.Binary{
							Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
						},
						Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!="},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "not equal on two expressions that doesnt lead to same type",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left: &expression.Binary{
							Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
							Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
							Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
						},
						Operator: token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.NOT_EQUAL, Lexeme: "!=", Line: 0},
				Message: "Operation != cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "greater on two numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.GREATER, Lexeme: ">"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "greater on one non numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 0},
				Message: "Operation > cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "greater on two non numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.GREATER, Lexeme: ">", Line: 0},
				Message: "Operation > cannot be performed on type String and type Bool",
			},
		},
		{
			name: "greater equal on two numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">="},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(false),
		},
		{
			name: "greater equal on one non numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Line: 0},
				Message: "Operation >= cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "greater equal on two non numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.GREATER_EQUAL, Lexeme: ">=", Line: 0},
				Message: "Operation >= cannot be performed on type String and type Bool",
			},
		},
		{
			name: "less on two numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.LESS, Lexeme: "<"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "less on one non numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Line: 0},
				Message: "Operation < cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "less on two non numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.LESS, Lexeme: "<", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.LESS, Lexeme: "<", Line: 0},
				Message: "Operation < cannot be performed on type String and type Bool",
			},
		},
		{
			name: "less equal on two numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<="},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			expected: literal.GenerateBoolLiteral(true),
		},
		{
			name: "less equal on one non numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Line: 0},
				Message: "Operation <= cannot be performed on type Numeric and type Bool",
			},
		},
		{
			name: "less equal on two non numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.LESS_EQUAL, Lexeme: "<=", Line: 0},
				Message: "Operation <= cannot be performed on type String and type Bool",
			},
		},
		{
			name: "minus on two numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			expected: literal.GenerateNumericLiteral(-1),
		},
		{
			name: "minus on one non numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: &literal.NullLiteral{}},
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "minus on two non numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.MINUS, Lexeme: "-", Line: 0},
				Message: "Operation - cannot be performed on type String and type Bool",
			},
		},
		{
			name: "plus on two numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			expected: literal.GenerateNumericLiteral(3),
		},
		{
			name: "plus on two strings",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
						Right:    &expression.Literal{Value: literal.GenerateStringLiteral("str")},
					}}},
			},
			expected: literal.GenerateStringLiteral("strstr"),
		},
		{
			name: "plus on one non numeric/string",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: &literal.NullLiteral{}},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
				Message: "Operation + cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "plus on two non numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateBoolLiteral(false)},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
				Message: "Operation + cannot be performed on type Bool and type Bool",
			},
		},
		{
			name: "plus on numeric and string",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 0},
				Message: "Operation + cannot be performed on type String and type Numeric",
			},
		},
		{
			name: "star on two numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.STAR, Lexeme: "*"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			expected: literal.GenerateNumericLiteral(2),
		},
		{
			name: "star on one non numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: &literal.NullLiteral{}},
						Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.STAR, Lexeme: "*", Line: 0},
				Message: "Operation * cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "star on two non numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.STAR, Lexeme: "*", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.STAR, Lexeme: "*", Line: 0},
				Message: "Operation * cannot be performed on type String and type Bool",
			},
		},
		{
			name: "slash on two numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
						Operator: token.Token{TokenType: token.SLASH, Lexeme: "/"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			expected: literal.GenerateNumericLiteral(0.5),
		},
		{
			name: "slash on one non numeric",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: &literal.NullLiteral{}},
						Operator: token.Token{TokenType: token.SLASH, Lexeme: "/", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(2)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.SLASH, Lexeme: "/", Line: 0},
				Message: "Operation / cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "slash on two non numerics",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left:     &expression.Literal{Value: literal.GenerateStringLiteral("str")},
						Operator: token.Token{TokenType: token.SLASH, Lexeme: "/", Line: 0},
						Right:    &expression.Literal{Value: literal.GenerateBoolLiteral(true)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.SLASH, Lexeme: "/", Line: 0},
				Message: "Operation / cannot be performed on type String and type Bool",
			},
		},
		{
			name: "test evaluate expression with defined variable",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					}}},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(1))
					return env
				}(),
			},
			expected: literal.GenerateNumericLiteral(2),
		},
		{
			name: "test evaluate expression with undefined variable",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
						},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+"},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Message: "Cannot get value of undefined variable \"a\"",
			},
		},
		{
			name: "test evaluate expression with null variable",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Binary{
						Left: &expression.Variable{
							Name: token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						},
						Operator: token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
						Right:    &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					}}},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &literal.NullLiteral{})
					return env
				}(),
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.PLUS, Lexeme: "+", Line: 1},
				Message: "Operation + cannot be performed on type Null and type Numeric",
			},
		},
		{
			name: "test assign to initialized variable",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Assignment{
						Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					}}},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", &literal.NullLiteral{})
					return env
				}(),
			},
			expected: literal.GenerateNumericLiteral(1),
		},
		{
			name: "test assign to uninitialized variable",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Assignment{
						Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
						Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					}}},
			},
			err: &exu_err.RuntimeError{
				Token:   token.Token{TokenType: token.IDENTIFIER, Lexeme: "a", Line: 1},
				Message: "Cannot assign to undefined variable \"a\"",
			},
		},
		{
			name: "test assign to defined numeric variable",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Assignment{
						Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					}}},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateNumericLiteral(2))
					return env
				}(),
			},
			expected: literal.GenerateNumericLiteral(1),
		},
		{
			name: "test assign to defined string variable",
			interpreter: Interpreter{
				Statements: []statement.Stmt{&statement.Expression{
					Expression: &expression.Assignment{
						Name:  token.Token{TokenType: token.IDENTIFIER, Lexeme: "a"},
						Value: &expression.Literal{Value: literal.GenerateNumericLiteral(1)},
					}}},
				env: func() environment.Environment {
					env := environment.GenerateEnvironment(nil)
					env.Define("a", literal.GenerateStringLiteral("str"))
					return env
				}(),
			},
			expected: literal.GenerateNumericLiteral(1),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Get expression from interpreter struct
			expr, ok := test.interpreter.Statements[0].(*statement.Expression)
			if !ok {
				t.Errorf("Cannot parse expression from statement in interpreter")
			}

			actual, err := test.interpreter.evaluate(expr.Expression)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}
