// AST NODES
// The grammar of Exu statements are defined in grammar.md
// The node definitions utilizes the visitor pattern following Lox's AST nodes

package expr

import (
	"github.com/rokkunbruv/internals/token"
)

type Visitor interface {
	VisitBinaryExpr(*Binary) (any, error)
	VisitGroupingExpr(*Grouping) (any, error)
	VisitLiteralExpr(*Literal) (any, error)
	VisitUnaryExpr(*Unary) (any, error)
}

type Expr interface {
	Accept(Visitor) (any, error)
}

type Binary struct {
	Left     Expr
	Operator token.Token
	Right    Expr
}

func (b *Binary) Accept(visitor Visitor) (any, error) {
	return visitor.VisitBinaryExpr(b)
}

type Grouping struct {
	Expression Expr
}

func (g *Grouping) Accept(visitor Visitor) (any, error) {
	return visitor.VisitGroupingExpr(g)
}

type Literal struct {
	Value token.Literal
}

func (l *Literal) Accept(visitor Visitor) (any, error) {
	return visitor.VisitLiteralExpr(l)
}

type Unary struct {
	Operator token.Token
	Right    Expr
}

func (u *Unary) Accept(visitor Visitor) (any, error) {
	return visitor.VisitUnaryExpr(u)
}
