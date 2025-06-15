package interpreter

import (
	"fmt"
	"io"
	"os"

	"github.com/rokkunbruv/internals/environment"
	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expression"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/statement"
	"github.com/rokkunbruv/internals/token"
)

var out io.Writer = os.Stdout

type Interpreter struct {
	Statements []statement.Stmt        // List of statements from source code
	env        environment.Environment // Environment the interpreter is currently in
}

// Executes the parsed AST
func (i *Interpreter) Interpret() error {
	i.env = environment.GenerateEnvironment(nil)

	for _, statement := range i.Statements {
		err := i.execute(statement)
		if err != nil {
			return err
		}
	}
	return nil
}

// Executes a statement
func (i *Interpreter) execute(stmt statement.Stmt) error {
	err := stmt.Accept(i)
	if err != nil {
		return err
	}
	return nil
}

// Evaluates an expression
func (i *Interpreter) evaluate(exp expression.Expr) (literal.Literal, error) {
	litObj, err := exp.Accept(i)
	if err != nil {
		return nil, err
	}

	lit, ok := litObj.(literal.Literal)
	if !ok {
		return nil, &exu_err.CastError{Message: "Evaluated result of expression does not result to a literal."}
	}

	return lit, err
}

func (i *Interpreter) VisitBlockStmt(stmt *statement.Block) error {
	// Copy the current env to a completely new outerEnv detached from the current
	// env's reference to save the states of the current env and its enclosings
	// This is to preserve the states defined in outerEnv prior to applying any
	// new state changes from the innermost scope

	// { outerEnv's scope
	// 	states defined here are preserved by outerEnv
	// 	{ current i.env's scope
	// 		any states added or changed here will not affect outerEnv
	// 	}
	// }
	outerEnv := environment.CopyEnvironment(i.env)

	// Shallow copy current environment to avoid recursive references
	enclosing := i.env
	i.env = environment.GenerateEnvironment(&enclosing)

	// Execute statements inside the block
	for _, statement := range stmt.Statements {
		err := i.execute(statement)
		if err != nil {
			return err
		}
	}

	// Return the scope back to the outer scope
	i.env = outerEnv

	return nil
}

func (i *Interpreter) VisitExpressionStmt(stmt *statement.Expression) error {
	_, err := i.evaluate(stmt.Expression)
	if err != nil {
		return err
	}
	return nil
}

func (i *Interpreter) VisitPrintStmt(stmt *statement.Print) error {
	val, err := i.evaluate(stmt.Expression)
	if err != nil {
		return err
	}

	fmt.Fprintln(out, val.ToString())

	return nil
}

func (i *Interpreter) VisitLetStmt(stmt *statement.Let) error {
	var value literal.Literal

	// If a variable is initialized to a value, set the value of the variable
	// to be that value, otherwise set it to null
	if stmt.Initializer != nil {
		// Explicitly declare err to avoid redeclaring value
		var err error

		value, err = i.evaluate(stmt.Initializer)
		if err != nil {
			return err
		}
	} else {
		value = &literal.NullLiteral{}
	}

	// Initialize variable
	i.env.Define(stmt.Name.Lexeme, value)

	return nil
}

func (i *Interpreter) VisitAssignmentExpr(exp *expression.Assignment) (any, error) {
	value, err := i.evaluate(exp.Value)
	if err != nil {
		return nil, err
	}

	err = i.env.Assign(exp.Name, value)
	if err != nil {
		return nil, err
	}

	return value, nil
}

func (i *Interpreter) VisitVariableExpr(exp *expression.Variable) (any, error) {
	return i.env.Get(exp.Name)
}

func (i *Interpreter) VisitLiteralExpr(exp *expression.Literal) (any, error) {
	return exp.Value, nil
}

func (i *Interpreter) VisitGroupingExpr(exp *expression.Grouping) (any, error) {
	val, err := i.evaluate(exp.Expression)
	if err != nil {
		return nil, err
	}

	return val, nil
}

func (i *Interpreter) VisitUnaryExpr(exp *expression.Unary) (any, error) {
	val, err := i.evaluate(exp.Right)
	if err != nil {
		return nil, err
	}

	switch exp.Operator.TokenType {
	case token.NOT:
		// Only allow boolean expressions to use !
		b, err := toBool(val)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v", exp.Operator.Lexeme, val.Type()),
			}
		}

		negateB := &literal.BoolLiteral{}
		negateB.SetVal(!b)

		return negateB, nil
	case token.MINUS:
		// Only allow numeric expressions to use -
		num, err := toNumeric(val)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v", exp.Operator.Lexeme, val.Type()),
			}
		}

		negativeNum := &literal.NumericLiteral{}
		negativeNum.SetVal(-num)

		return negativeNum, nil
	}

	return nil, nil
}

func (i *Interpreter) VisitBinaryExpr(exp *expression.Binary) (any, error) {
	left, err := i.evaluate(exp.Left)
	if err != nil {
		return nil, err
	}

	right, err := i.evaluate(exp.Right)
	if err != nil {
		return nil, err
	}

	switch exp.Operator.TokenType {
	// Only allow boolean and null expressions to use equality operators
	case token.NOT_EQUAL:
		result, err := isEqual(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newBool := &literal.BoolLiteral{}
		newBool.SetVal(!result)

		return newBool, nil

	case token.EQUAL:
		result, err := isEqual(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newBool := &literal.BoolLiteral{}
		newBool.SetVal(result)

		return newBool, nil

	// Only allow numeric expressions to use comparison operators
	case token.GREATER:
		lNum, rNum, err := toTwoNumerics(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newBool := &literal.BoolLiteral{}
		newBool.SetVal(lNum > rNum)

		return newBool, nil

	case token.GREATER_EQUAL:
		lNum, rNum, err := toTwoNumerics(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newBool := &literal.BoolLiteral{}
		newBool.SetVal(lNum >= rNum)

		return newBool, nil

	case token.LESS:
		lNum, rNum, err := toTwoNumerics(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newBool := &literal.BoolLiteral{}
		newBool.SetVal(lNum < rNum)

		return newBool, nil

	case token.LESS_EQUAL:
		lNum, rNum, err := toTwoNumerics(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newBool := &literal.BoolLiteral{}
		newBool.SetVal(lNum <= rNum)

		return newBool, nil

	// Only allow numeric expressions to use arithmetic operators
	// except for strings which can use the + operator for concatenation
	case token.MINUS:
		lNum, rNum, err := toTwoNumerics(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newNum := &literal.NumericLiteral{}
		newNum.SetVal(lNum - rNum)

		return newNum, nil

	case token.PLUS:
		lNum, rNum, err := toTwoNumerics(left, right)
		if err == nil {
			newNum := &literal.NumericLiteral{}
			newNum.SetVal(lNum + rNum)

			return newNum, nil
		}

		lStr, rStr, err := toTwoStrs(left, right)
		if err == nil {
			newStr := &literal.StringLiteral{}
			newStr.SetVal(lStr + rStr)

			return newStr, nil
		}

		return nil, &exu_err.RuntimeError{
			Token:   exp.Operator,
			Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
		}

	case token.STAR:
		lNum, rNum, err := toTwoNumerics(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newNum := &literal.NumericLiteral{}
		newNum.SetVal(lNum * rNum)

		return newNum, nil

	case token.SLASH:
		lNum, rNum, err := toTwoNumerics(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newNum := &literal.NumericLiteral{}
		newNum.SetVal(lNum / rNum)

		return newNum, nil
	}

	return nil, nil
}

// Utility function to check if two literals of the same type are equal;
// throws an error if two literals are not of the same type (except for null values)
func isEqual(lit1 literal.Literal, lit2 literal.Literal) (bool, error) {
	// Check if either expressions are null
	_, ok1 := lit1.(*literal.NullLiteral)
	_, ok2 := lit2.(*literal.NullLiteral)

	// Return true if both expressions are null
	// else return false if either expression is null
	if ok1 && ok2 {
		return true, nil
	} else if ok1 || ok2 {
		return false, nil
	}

	// Equality check for two bool literals
	bool1, bool2, err := toTwoBools(lit1, lit2)
	if err == nil {
		return bool1 == bool2, nil
	}

	// Equality check for two string literals
	str1, str2, err := toTwoStrs(lit1, lit2)
	if err == nil {
		return str1 == str2, nil
	}

	// Equality check for two numeric literals
	num1, num2, err := toTwoNumerics(lit1, lit2)
	if err == nil {
		return num1 == num2, nil
	}

	return false, fmt.Errorf("")
}

// Utility function to convert a literal to a numeric literal
func toNumeric(lit literal.Literal) (float64, error) {
	num, ok := lit.(*literal.NumericLiteral)
	if !ok {
		return 0, fmt.Errorf("")
	}
	return num.Val()
}

// Utility function to convert two literals to two numeric literals
func toTwoNumerics(lit1 literal.Literal, lit2 literal.Literal) (float64, float64, error) {
	num1, err := toNumeric(lit1)
	if err != nil {
		return 0, 0, err
	}

	num2, err := toNumeric(lit2)
	if err != nil {
		return 0, 0, err
	}

	return num1, num2, nil
}

// Utility function to convert a literal to a bool literal
func toBool(lit literal.Literal) (bool, error) {
	boolean, ok := lit.(*literal.BoolLiteral)
	if !ok {
		return false, fmt.Errorf("")
	}
	return boolean.Val()
}

// Utility function to convert two literals to two bool literals
func toTwoBools(lit1 literal.Literal, lit2 literal.Literal) (bool, bool, error) {
	bool1, err := toBool(lit1)
	if err != nil {
		return false, false, err
	}

	bool2, err := toBool(lit2)
	if err != nil {
		return false, false, err
	}

	return bool1, bool2, nil
}

// Utility function to convert a literal to a string literal
func toStr(lit literal.Literal) (string, error) {
	str, ok := lit.(*literal.StringLiteral)
	if !ok {
		return "", fmt.Errorf("")
	}
	return str.Val()
}

// Utility function to convert two literals to two string literals
func toTwoStrs(lit1 literal.Literal, lit2 literal.Literal) (string, string, error) {
	str1, err := toStr(lit1)
	if err != nil {
		return "", "", err
	}

	str2, err := toStr(lit2)
	if err != nil {
		return "", "", err
	}

	return str1, str2, nil
}
