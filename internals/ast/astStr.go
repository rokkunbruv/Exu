package ast

import (
	"fmt"
	"strings"

	"github.com/rokkunbruv/internals/expr"
)

type AstString struct {
	indent string    // Sets the indentation string as the visitor traverses to the AST
	Expr   expr.Expr // The expression tree to be converted to a string
}

func (a *AstString) Generate() (string, error) {
	visitor := &AstString{indent: ""}

	strObj, err := a.Expr.Accept(visitor)
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
	return generateSubTree(a, expr.Operator.Lexeme, expr.Left, expr.Right)
}

func (a *AstString) VisitGroupingExpr(expr *expr.Grouping) (any, error) {
	return generateSubTree(a, "()", expr.Expression)
}

func (a *AstString) VisitLiteralExpr(expr *expr.Literal) (any, error) {
	return expr.Value.ToString(), nil
}

func (a *AstString) VisitUnaryExpr(expr *expr.Unary) (any, error) {
	return generateSubTree(a, expr.Operator.Lexeme, expr.Right)
}

func generateSubTree(a *AstString, parentStr string, exprs ...expr.Expr) (string, error) {
	var buf string

	for i, exp := range exprs {
		// Add indentation string
		if i == len(exprs)-1 {
			a.indent += "   "
		} else {
			a.indent += "│  "
		}

		expObj, err := exp.Accept(a)
		if err != nil {
			return "", fmt.Errorf("%w\ninvoked from generateSubTree(%v, %v, %v)", err, a, parentStr, exprs)
		}
		expStr, ok := expObj.(string)
		if !ok {
			return "", fmt.Errorf("invalid parsing: object is not string")
		}

		// Undo indentation string
		if i == len(exprs)-1 {
			a.indent = strings.TrimSuffix(a.indent, "   ")
		} else {
			a.indent = strings.TrimSuffix(a.indent, "│  ")
		}

		// Set branch junction
		branch := "├─"
		if i == len(exprs)-1 {
			branch = "└─"
		}

		buf += fmt.Sprintf("\n%v%v %v", a.indent, branch, expStr)
	}

	return fmt.Sprintf("%v%v", parentStr, buf), nil
}
