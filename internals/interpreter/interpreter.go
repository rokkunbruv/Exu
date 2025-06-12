package interpreter

import (
	"fmt"

	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expr"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
)

type Interpreter struct {
	Expression expr.Expr
}

func (i *Interpreter) Interpret() error {
	val, err := i.evaluate(i.Expression)
	if err != nil {
		return err
	}

	// Print the value of the evaluated expression
	fmt.Println(val.ToString())

	return nil
}

func (i *Interpreter) evaluate(exp expr.Expr) (literal.Literal, error) {
	litObj, err := exp.Accept(i)
	if err != nil {
		return nil, err
	}

	lit, ok := litObj.(literal.Literal)
	if !ok {
		return nil, fmt.Errorf("")
	}

	return lit, err
}

func (i *Interpreter) VisitLiteralExpr(exp *expr.Literal) (any, error) {
	return exp.Value, nil
}

func (i *Interpreter) VisitGroupingExpr(exp *expr.Grouping) (any, error) {
	val, err := i.evaluate(exp.Expression)
	if err != nil {
		return nil, err
	}

	return val, nil
}

func (i *Interpreter) VisitUnaryExpr(exp *expr.Unary) (any, error) {
	val, err := i.evaluate(exp.Right)
	if err != nil {
		return nil, err
	}

	switch exp.Operator.TokenType {
	case token.NOT:
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

func (i *Interpreter) VisitBinaryExpr(exp *expr.Binary) (any, error) {
	left, err := i.evaluate(exp.Left)
	if err != nil {
		return nil, err
	}

	right, err := i.evaluate(exp.Right)
	if err != nil {
		return nil, err
	}

	switch exp.Operator.TokenType {
	case token.NOT_EQUAL:
		lBool, rBool, err := toTwoBools(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newBool := &literal.BoolLiteral{}
		newBool.SetVal(lBool != rBool)

		return newBool, nil

	case token.EQUAL:
		lBool, rBool, err := toTwoBools(left, right)
		if err != nil {
			return nil, &exu_err.RuntimeError{
				Token:   exp.Operator,
				Message: fmt.Sprintf("Operation %v cannot be performed on type %v and type %v", exp.Operator.Lexeme, left.Type(), right.Type()),
			}
		}

		newBool := &literal.BoolLiteral{}
		newBool.SetVal(lBool == rBool)

		return newBool, nil

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

func toNumeric(lit literal.Literal) (float64, error) {
	num, ok := lit.(*literal.NumericLiteral)
	if !ok {
		return 0, fmt.Errorf("")
	}
	return num.Val()
}

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

func toBool(lit literal.Literal) (bool, error) {
	boolean, ok := lit.(*literal.BoolLiteral)
	if !ok {
		return false, fmt.Errorf("")
	}
	return boolean.Val()
}

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

func toStr(lit literal.Literal) (string, error) {
	str, ok := lit.(*literal.StringLiteral)
	if !ok {
		return "", fmt.Errorf("")
	}
	return str.Val()
}

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
