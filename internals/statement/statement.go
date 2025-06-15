// AST STATEMENT NODES
// The grammar of Exu statements are defined in grammar.md
// The node definitions utilizes the visitor pattern following Lox's AST nodes

package statement

import (
	"github.com/rokkunbruv/internals/expression"
	"github.com/rokkunbruv/internals/token"
)

type Visitor interface {
	VisitExpressionStmt(*Expression) error
	VisitPrintStmt(*Print) error
	VisitLetStmt(*Let) error
	VisitBlockStmt(*Block) error
}

type Stmt interface {
	Accept(Visitor) error
}

type Expression struct {
	Expression expression.Expr
}

func (e *Expression) Accept(visitor Visitor) error {
	return visitor.VisitExpressionStmt(e)
}

type Print struct {
	Expression expression.Expr
}

func (p *Print) Accept(visitor Visitor) error {
	return visitor.VisitPrintStmt(p)
}

type Let struct {
	Name        token.Token
	Initializer expression.Expr
}

func (l *Let) Accept(visitor Visitor) error {
	return visitor.VisitLetStmt(l)
}

type Block struct {
	Statements []Stmt
}

func (b *Block) Accept(visitor Visitor) error {
	return visitor.VisitBlockStmt(b)
}
