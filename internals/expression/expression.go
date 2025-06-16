// AST EXPRESSION NODES
// The grammar of Exu expressions are defined in grammar.md
// The node definitions utilizes the visitor pattern following Lox's AST nodes

package expression

import (
	literal "github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
)

type Visitor interface {
	VisitBinaryExpr(*Binary) (any, error)
	VisitGroupingExpr(*Grouping) (any, error)
	VisitLiteralExpr(*Literal) (any, error)
	VisitUnaryExpr(*Unary) (any, error)
	VisitVariableExpr(*Variable) (any, error)
	VisitAssignmentExpr(*Assignment) (any, error)
	VisitLogicalExpr(*Logical) (any, error)
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
	Value literal.Literal
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

type Variable struct {
	Name token.Token
}

func (v *Variable) Accept(visitor Visitor) (any, error) {
	return visitor.VisitVariableExpr(v)
}

type Assignment struct {
	Name  token.Token
	Value Expr
}

func (a *Assignment) Accept(visitor Visitor) (any, error) {
	return visitor.VisitAssignmentExpr(a)
}

type Logical struct {
	Left     Expr
	Operator token.Token
	Right    Expr
}

func (l *Logical) Accept(visitor Visitor) (any, error) {
	return visitor.VisitLogicalExpr(l)
}
