// The parser parses the tokens list in accordance to the defined grammar in grammar.md

package parser

import (
	"fmt"

	"github.com/rokkunbruv/internals/expr"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/token"
)

// Parser struct to keep track of the
// tokens list and curr pointer
// as it generates the AST
type Parser struct {
	tokens []token.Token
	curr   int
}

// Parses the tokens list to its equivalent AST
func Parse(tokens []token.Token) (expr.Expr, error) {
	if len(tokens) == 0 {
		return nil, fmt.Errorf("tokens list is empty")
	}

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

	// Check for EQUAL or NOT_EQUAL tokens
	isEquality, err := p.match(token.EQUAL, token.NOT_EQUAL)
	if err != nil {
		return nil, err
	}

	for isEquality {
		operator, err := p.previous()
		if err != nil {
			return nil, err
		}

		right, err := p.comparison()
		if err != nil {
			return nil, err
		}

		exp = &expr.Binary{Left: exp, Operator: operator, Right: right}

		// Update isEquality
		isEquality, err = p.match(token.EQUAL, token.NOT_EQUAL)
		if err != nil {
			return nil, err
		}
	}

	return exp, nil
}

func (p *Parser) comparison() (expr.Expr, error) {
	exp, err := p.term()
	if err != nil {
		return nil, err
	}

	// Check for comparison tokens
	isComparison, err := p.match(token.GREATER, token.GREATER_EQUAL, token.LESS, token.LESS_EQUAL)
	if err != nil {
		return nil, err
	}

	for isComparison {
		operator, err := p.previous()
		if err != nil {
			return nil, err
		}

		right, err := p.term()
		if err != nil {
			return nil, err
		}

		exp = &expr.Binary{Left: exp, Operator: operator, Right: right}

		// Update isComparison
		isComparison, err = p.match(token.GREATER, token.GREATER_EQUAL, token.LESS, token.LESS_EQUAL)
		if err != nil {
			return nil, err
		}
	}

	return exp, nil
}

func (p *Parser) term() (expr.Expr, error) {
	exp, err := p.factor()
	if err != nil {
		return nil, err
	}

	// Check for MINUS or PLUS tokens
	isTermOp, err := p.match(token.MINUS, token.PLUS)
	if err != nil {
		return nil, err
	}

	for isTermOp {
		operator, err := p.previous()
		if err != nil {
			return nil, err
		}

		right, err := p.factor()
		if err != nil {
			return nil, err
		}

		exp = &expr.Binary{Left: exp, Operator: operator, Right: right}

		// Update isTermOp
		isTermOp, err = p.match(token.MINUS, token.PLUS)
		if err != nil {
			return nil, err
		}
	}

	return exp, nil
}

func (p *Parser) factor() (expr.Expr, error) {
	exp, err := p.unary()
	if err != nil {
		return nil, err
	}

	// Check for SLASH or STAR tokens
	isFactorOp, err := p.match(token.SLASH, token.STAR)
	if err != nil {
		return nil, err
	}

	for isFactorOp {
		operator, err := p.previous()
		if err != nil {
			return nil, err
		}

		right, err := p.unary()
		if err != nil {
			return nil, err
		}

		exp = &expr.Binary{Left: exp, Operator: operator, Right: right}

		// Update isFactorOp
		isFactorOp, err = p.match(token.SLASH, token.STAR)
		if err != nil {
			return nil, err
		}
	}

	return exp, nil
}

func (p *Parser) unary() (expr.Expr, error) {
	// Check for NOT or MINUS tokens
	isUnary, err := p.match(token.NOT, token.MINUS)
	if err != nil {
		return nil, err
	}

	if isUnary {
		operator, err := p.previous()
		if err != nil {
			return nil, err
		}

		right, err := p.unary()
		if err != nil {
			return nil, err
		}

		return &expr.Unary{Operator: operator, Right: right}, nil
	}

	return p.primary()
}

func (p *Parser) primary() (expr.Expr, error) {
	// Check for FALSE token
	isFalse, err := p.match(token.FALSE)
	if err != nil {
		return nil, err
	}
	if isFalse {
		return &expr.Literal{Value: func() *literal.BoolLiteral {
			lit := &literal.BoolLiteral{}
			lit.SetVal(false)
			return lit
		}()}, nil
	}

	// Check for TRUE token
	isTrue, err := p.match(token.TRUE)
	if err != nil {
		return nil, err
	}
	if isTrue {
		return &expr.Literal{Value: func() *literal.BoolLiteral {
			lit := &literal.BoolLiteral{}
			lit.SetVal(true)
			return lit
		}()}, nil
	}

	// Check for NULL token
	isNull, err := p.match(token.NULL)
	if err != nil {
		return nil, err
	}
	if isNull {
		return &expr.Literal{Value: nil}, nil
	}

	// Check for NUMERIC or STRING tokens
	isNumOrStr, err := p.match(token.NUMERIC, token.STRING)
	if err != nil {
		return nil, err
	}
	if isNumOrStr {
		prev, err := p.previous()
		if err != nil {
			return nil, err
		}
		return &expr.Literal{Value: prev.Literal}, nil
	}

	// Check for LEFT_PAREN token
	isLeftParen, err := p.match(token.LEFT_PAREN)
	if err != nil {
		return nil, err
	}
	if isLeftParen {
		exp, err := p.expression()
		if err != nil {
			return nil, err
		}

		// Check for RIGHT_PAREN token if preceded with LEFT_PAREN
		_, err = p.consume(token.RIGHT_PAREN, "Expect ')' after expression")
		if err != nil {
			return nil, err
		}

		return &expr.Grouping{Expression: exp}, nil
	}

	// Throws an error on the current token
	// if it doesnt match with any of the
	// above conditions
	currToken, err := p.peek()
	if err != nil {
		return nil, err
	}

	return nil, fmt.Errorf("expect expression at %v", currToken.Lexeme)
}

// Return true and moves the curr ptr to the next
// element in tokens if the curr token matches with
// any of the listed types
func (p *Parser) match(types ...token.TokenType) (bool, error) {
	for _, t := range types {
		isType, err := p.check(t)
		if err != nil {
			return false, err
		}
		if isType {
			_, err := p.advance()
			if err != nil {
				return false, err
			}
			return true, nil
		}
	}

	return false, nil
}

// Returns the curr token and moves the curr ptr to
// the next token if the curr token matches with
// the type
func (p *Parser) consume(t token.TokenType, errorMsg string) (token.Token, error) {
	isType, err := p.check(t)
	if err != nil {
		return token.Token{}, err
	}

	if isType {
		currToken, err := p.advance()
		if err != nil {
			return token.Token{}, err
		}
		return currToken, nil
	}

	// Throws an error if there is a type mismatch
	return token.Token{}, fmt.Errorf("%s", errorMsg)
}

// Checks if the current token is of a specific type
func (p *Parser) check(t token.TokenType) (bool, error) {
	atEnd, err := p.isAtEnd()
	if err != nil {
		return false, err
	}
	if atEnd {
		return false, nil
	}

	currToken, err := p.peek()
	if err != nil {
		return false, err
	}

	return currToken.TokenType == t, nil
}

// Pops the current token and moves curr ptr
// to the next token
func (p *Parser) advance() (token.Token, error) {
	atEnd, err := p.isAtEnd()
	if err != nil {
		return token.Token{}, err
	}
	// Moves the curr ptr to the next token
	// until it reaches EOF token in which
	// the curr ptr will stop moving at that point
	if !atEnd {
		p.curr++
	}

	prev, err := p.previous()
	if err != nil {
		return token.Token{}, err
	}

	return prev, nil
}

func (p *Parser) isAtEnd() (bool, error) {
	currToken, err := p.peek()
	if err != nil {
		return false, err
	}
	return currToken.TokenType == token.EOF, nil
}

// Gets current token
func (p *Parser) peek() (token.Token, error) {
	if !p.isCurrInBounds() {
		return token.Token{}, fmt.Errorf("invalid access to tokens list: index out of bounds")
	}
	return p.tokens[p.curr], nil
}

// Gets previous token
func (p *Parser) previous() (token.Token, error) {
	// We shift the bounds one unit to the right
	// since we are accessing the previous token
	if p.curr < 1 || p.curr >= len(p.tokens)+1 {
		return token.Token{}, fmt.Errorf("invalid access to tokens list: index out of bounds")
	}
	return p.tokens[p.curr-1], nil
}

// Utility function to check if curr is a
// valid index in tokens
func (p *Parser) isCurrInBounds() bool {
	if p.curr < 0 || p.curr >= len(p.tokens) {
		return false
	}
	return true
}
