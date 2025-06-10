package parser

import (
	"fmt"

	"github.com/rokkunbruv/internals/expr"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
)

type Parser struct {
	tokens []token.Token
	curr   int
}

func Parse(tokens []token.Token) (expr.Expr, error) {
	parserObj := Parser{tokens: tokens}
	return parserObj.expression()
}

func (p *Parser) expression() (expr.Expr, error) {
	return p.equality()
}

func (p *Parser) equality() (expr.Expr, error) {
	exp, err := p.comparison()
	if err != nil {
		return nil, err
	}

	for p.match(token.EQUAL, token.NOT_EQUAL) {
		operator := p.previous()
		right, err := p.comparison()
		if err != nil {
			return nil, err
		}
		exp = &expr.Binary{Left: exp, Operator: operator, Right: right}
	}

	return exp, nil
}

func (p *Parser) comparison() (expr.Expr, error) {
	exp, err := p.term()
	if err != nil {
		return nil, err
	}

	for p.match(token.GREATER, token.GREATER_EQUAL, token.LESS, token.LESS_EQUAL) {
		operator := p.previous()
		right, err := p.term()
		if err != nil {
			return nil, err
		}
		exp = &expr.Binary{Left: exp, Operator: operator, Right: right}
	}

	return exp, nil
}

func (p *Parser) term() (expr.Expr, error) {
	exp, err := p.factor()
	if err != nil {
		return nil, err
	}

	for p.match(token.MINUS, token.PLUS) {
		operator := p.previous()
		right, err := p.factor()
		if err != nil {
			return nil, err
		}
		exp = &expr.Binary{Left: exp, Operator: operator, Right: right}
	}

	return exp, nil
}

func (p *Parser) factor() (expr.Expr, error) {
	exp, err := p.unary()
	if err != nil {
		return nil, err
	}

	for p.match(token.SLASH, token.STAR) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		exp = &expr.Binary{Left: exp, Operator: operator, Right: right}
	}

	return exp, nil
}

func (p *Parser) unary() (expr.Expr, error) {
	if p.match(token.NOT, token.MINUS) {
		operator := p.previous()
		right, err := p.unary()
		if err != nil {
			return nil, err
		}
		return &expr.Unary{Operator: operator, Right: right}, nil
	}

	return p.primary()
}

func (p *Parser) primary() (expr.Expr, error) {
	if p.match(token.FALSE) {
		return &expr.Literal{Value: func() *literal.BoolLiteral {
			lit := &literal.BoolLiteral{}
			lit.SetVal(false)
			return lit
		}()}, nil
	}
	if p.match(token.TRUE) {
		return &expr.Literal{Value: func() *literal.BoolLiteral {
			lit := &literal.BoolLiteral{}
			lit.SetVal(true)
			return lit
		}()}, nil
	}
	if p.match(token.NULL) {
		return &expr.Literal{Value: nil}, nil
	}

	if p.match(token.NUMERIC, token.STRING) {
		prev := p.previous()
		return &expr.Literal{Value: prev.Literal}, nil
	}

	if p.match(token.LEFT_PAREN) {
		exp, err := p.expression()
		if err != nil {
			return nil, err
		}
		p.consume(token.RIGHT_PAREN, "Expect ')' after expression")
		return &expr.Grouping{Expression: exp}, nil
	}

	currToken := p.peek()
	return nil, fmt.Errorf("expect expression at %v", currToken.ToString())
}

func (p *Parser) match(types ...token.TokenType) bool {
	for _, t := range types {
		if p.check(t) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *Parser) consume(t token.TokenType, errorMsg string) (token.Token, error) {
	if p.check(t) {
		return p.advance(), nil
	}
	return token.Token{}, fmt.Errorf("%s", errorMsg)
}

func (p *Parser) check(t token.TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().TokenType == t
}

func (p *Parser) advance() token.Token {
	if !p.isAtEnd() {
		p.curr++
	}
	return p.previous()
}

func (p *Parser) isAtEnd() bool {
	return p.peek().TokenType == token.EOF
}

func (p *Parser) peek() token.Token {
	return p.tokens[p.curr]
}

func (p *Parser) previous() token.Token {
	return p.tokens[p.curr-1]
}
