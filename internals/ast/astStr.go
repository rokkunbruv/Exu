package ast

import (
	"fmt"

	"github.com/rokkunbruv/internals/expr"
	"github.com/rokkunbruv/internals/token"
)

type AstString struct{}

// Converts an AST to its string representation
// For this string representation implementation,
// trees and subtrees are enclosed in parentheses
// wherein the first item represents the parent
// of that tree/subtree
func main() {
	expression := &expr.Binary{
		Left: &expr.Unary{
			Operator: token.Token{
				TokenType: token.MINUS, Lexeme: "-", Literal: token.Literal{IsNull: true}, Line: 1,
			},
			Right: &expr.Literal{
				Value: token.Literal{
					Type: token.NUMERIC_LITERAL, DoubleVal: 123,
				},
			},
		},
		Operator: token.Token{
			TokenType: token.STAR, Lexeme: "*", Literal: token.Literal{IsNull: true}, Line: 1,
		},
		Right: &expr.Grouping{
			Expression: &expr.Literal{
				Value: token.Literal{
					Type: token.NUMERIC_LITERAL, DoubleVal: 45.67,
				},
			},
		},
	}

	fmt.Println(toString(expression))
}

func toString(expr expr.Expr) (string, error) {
	visitor := &AstString{}

	strObj, err := expr.Accept(visitor)
	if err != nil {
		return "", err
	}
	str, ok := strObj.(string)
	if !ok {
		return "", fmt.Errorf("invalid parsing: object is not string")
	}

	return str, nil
}

func (a *AstString) VisitBinaryExpr(expr *expr.Binary) (any, error) {
	return parenthesize(expr.Operator.Lexeme, expr.Left, expr.Right)
}

func (a *AstString) VisitGroupingExpr(expr *expr.Grouping) (any, error) {
	return parenthesize("group", expr.Expression)
}

func (a *AstString) VisitLiteralExpr(expr *expr.Literal) (any, error) {
	return expr.Value.ToString()
}

func (a *AstString) VisitUnaryExpr(expr *expr.Unary) (any, error) {
	return parenthesize(expr.Operator.Lexeme, expr.Right)
}

func parenthesize(name string, exprs ...expr.Expr) (string, error) {
	visitor := &AstString{}
	var buf string

	for _, exp := range exprs {
		expObj, err := exp.Accept(visitor)
		if err != nil {
			return "", err
		}
		expStr, ok := expObj.(string)
		if !ok {
			return "", fmt.Errorf("invalid parsing: object is not string")
		}
		buf += fmt.Sprintf(" %v", expStr)
	}

	return fmt.Sprintf("(%v%v)", name, buf), nil
}
