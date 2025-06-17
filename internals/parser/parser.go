// The parser parses the tokens list in accordance to the defined grammar in grammar.md

package parser

import (
	"fmt"
	"slices"

	calltype "github.com/rokkunbruv/internals/call_type"
	exu_err "github.com/rokkunbruv/internals/err"
	"github.com/rokkunbruv/internals/expression"
	"github.com/rokkunbruv/internals/literal"
	"github.com/rokkunbruv/internals/statement"
	"github.com/rokkunbruv/internals/token"
)

// Parser struct to keep track of the
// tokens list and curr pointer
// as it generates the AST
type Parser struct {
	Tokens []token.Token
	Curr   int
}

// Parses the tokens list to its equivalent AST
func (p *Parser) Parse() ([]statement.Stmt, error) {
	statements := []statement.Stmt{}

	atEnd, err := p.isAtEnd()
	if err != nil {
		return nil, err
	}

	for !atEnd {
		statement, err := p.declaration()
		if err != nil {
			return nil, err
		}

		statements = append(statements, statement)

		atEnd, err = p.isAtEnd()
		if err != nil {
			return nil, err
		}
	}

	return statements, nil
}

func (p *Parser) declaration() (statement.Stmt, error) {
	// Check let keyword
	isLet, err := p.match(token.LET)
	if err != nil {
		// Exits expression tree
		sync_err := p.synchronize()

		if sync_err != nil {
			return nil, sync_err
		}

		return nil, err
	}

	if isLet {
		return p.varDeclaration()
	}

	// Check fn keyword
	isFn, err := p.match(token.FN)
	if err != nil {
		// Exits expression tree
		sync_err := p.synchronize()

		if sync_err != nil {
			return nil, sync_err
		}

		return nil, err
	}

	if isFn {
		return p.funcDeclaration(string(string(calltype.FUNCTION)))
	}

	return p.statement()
}

func (p *Parser) funcDeclaration(kind string) (statement.Stmt, error) {
	// Get function name
	name, err := p.consume(token.IDENTIFIER, fmt.Sprintf("Expected %v name", kind))
	if err != nil {
		return nil, err
	}

	// Get function parameters
	if _, err := p.consume(token.LEFT_PAREN, fmt.Sprintf("Expected \"(\" after %v name", kind)); err != nil {
		return nil, err
	}

	parameters := []token.Token{}

	if isRightParen, err := p.check(token.RIGHT_PAREN); err != nil {
		return nil, err
	} else if !isRightParen {
		// Execute loop first before evaluating condition
		// (stored in isComma)
		isComma := true

		for isComma {
			if len(parameters) >= 255 {
				currToken, err := p.peek()
				if err != nil {
					return nil, err
				}

				return nil, &exu_err.SyntaxError{
					Token:   currToken,
					Message: fmt.Sprintf("A %v cannot have more than 255 arguments", kind),
				}
			}

			parameter, err := p.consume(token.IDENTIFIER, "Expected parameter name")
			if err != nil {
				return nil, err
			}

			parameters = append(parameters, parameter)

			// Update isComma
			isComma, err = p.match(token.COMMA)
			if err != nil {
				return nil, err
			}
		}
	}

	if _, err := p.consume(token.RIGHT_PAREN, "Expected \")\" after parameters"); err != nil {
		return nil, err
	}

	// Get function block
	if _, err := p.consume(token.LEFT_BRACE, fmt.Sprintf("Expected \"{\" before %v parameters", kind)); err != nil {
		return nil, err
	}

	body, err := p.block()
	if err != nil {
		return nil, err
	}

	return &statement.Function{
		Name:   name,
		Params: parameters,
		Body:   body,
	}, nil
}

// Parses variable declarations
func (p *Parser) varDeclaration() (statement.Stmt, error) {
	varName, err := p.consume(token.IDENTIFIER, "Expected variable name")
	if err != nil {
		return nil, err
	}

	var initializer expression.Expr

	isColon, err := p.match(token.COLON)
	if err != nil {
		return nil, err
	}

	if isColon {
		initializer, err = p.expression()
		if err != nil {
			return nil, err
		}
	}

	_, err = p.consume(token.SEMICOLON, "Expected \";\" after variable declaration.")
	if err != nil {
		return nil, err
	}

	return &statement.Let{Name: varName, Initializer: initializer}, nil
}

func (p *Parser) statement() (statement.Stmt, error) {
	// Check for if keyword
	if isIf, err := p.match(token.IF); isIf {
		return p.ifStatement()
	} else if err != nil {
		return nil, err
	}

	// Check for for keyword
	if isFor, err := p.match(token.FOR); isFor {
		return p.forStatement()
	} else if err != nil {
		return nil, err
	}

	// Check for print keyword
	isPrint, err := p.match(token.PRINT)
	if err != nil {
		return nil, err
	}

	if isPrint {
		return p.printStatement()
	}

	// Check for while keyword
	isWhile, err := p.match(token.WHILE)
	if err != nil {
		return nil, err
	}

	if isWhile {
		return p.whileStatement()
	}

	// Check for return keyword
	isReturn, err := p.match(token.RETURN)
	if err != nil {
		return nil, err
	}

	if isReturn {
		return p.returnStatement()
	}

	// Check for blocks
	isBlock, err := p.match(token.LEFT_BRACE)
	if err != nil {
		return nil, err
	}

	if isBlock {
		statements, err := p.block()
		if err != nil {
			return nil, err
		}

		return &statement.Block{Statements: statements}, nil
	}

	return p.exprStatement()
}

func (p *Parser) returnStatement() (statement.Stmt, error) {
	keyword, err := p.previous()
	if err != nil {
		return nil, err
	}

	// Parse return expression
	var value expression.Expr
	if isSemicolon, err := p.check(token.SEMICOLON); err != nil {
		return nil, err
	} else if !isSemicolon {
		value, err = p.expression()
		if err != nil {
			return nil, err
		}
	} else {
		value = nil
	}

	_, err = p.consume(token.SEMICOLON, "Expected \";\" after return value")
	if err != nil {
		return nil, err
	}

	return &statement.Return{
		Keyword: keyword,
		Value:   value,
	}, nil
}

func (p *Parser) forStatement() (statement.Stmt, error) {
	if _, err := p.consume(token.LEFT_PAREN, "Expected \"(\" after \"for\""); err != nil {
		return nil, err
	}

	// Check initializer
	var initializer statement.Stmt
	if isSemicolon, err := p.match(token.SEMICOLON); err != nil {
		return nil, err
	} else if isSemicolon {
		initializer = nil
	} else if isLet, err := p.match(token.LET); err != nil {
		return nil, err
	} else if isLet {
		initializer, err = p.varDeclaration()
		if err != nil {
			return nil, err
		}
	} else {
		initializer, err = p.exprStatement()
		if err != nil {
			return nil, err
		}
	}

	// Check condition initializer
	var condition expression.Expr
	if isSemicolon, err := p.check(token.SEMICOLON); err != nil {
		return nil, err
	} else if !isSemicolon {
		condition, err = p.expression()
		if err != nil {
			return nil, err
		}
	} else {
		condition = nil
	}

	if _, err := p.consume(token.SEMICOLON, "Expected \";\" after condition of for loop"); err != nil {
		return nil, err
	}

	// Check iterator initializer
	var iterator expression.Expr
	if isRightParen, err := p.check(token.RIGHT_PAREN); err != nil {
		return nil, err
	} else if !isRightParen {
		iterator, err = p.expression()
		if err != nil {
			return nil, err
		}
	} else {
		iterator = nil
	}

	if _, err := p.consume(token.RIGHT_PAREN, "Expected \")\" after for loop clause"); err != nil {
		return nil, err
	}

	// Check body
	body, err := p.statement()
	if err != nil {
		return nil, err
	}

	// Convert for loop statement to its equivalent while loop
	if iterator != nil {
		body = &statement.Block{
			Statements: []statement.Stmt{
				body,
				&statement.Expression{
					Expression: iterator,
				},
			},
		}
	}

	if condition == nil {
		condition = &expression.Literal{
			Value: literal.GenerateBoolLiteral(true),
		}
	}
	body = &statement.While{
		Condition: condition,
		Body:      body,
	}

	if initializer != nil {
		body = &statement.Block{
			Statements: []statement.Stmt{
				initializer,
				body,
			},
		}
	}

	return body, nil
}

func (p *Parser) whileStatement() (statement.Stmt, error) {
	// Check while condition expression
	if _, err := p.consume(token.LEFT_PAREN, "Expected \"(\" after \"while\""); err != nil {
		return nil, err
	}
	condition, err := p.expression()
	if err != nil {
		return nil, err
	}
	if _, err := p.consume(token.RIGHT_PAREN, "Expected \")\" after while condition"); err != nil {
		return nil, err
	}

	// Check while loop body
	body, err := p.statement()
	if err != nil {
		return nil, err
	}

	return &statement.While{Condition: condition, Body: body}, nil
}

func (p *Parser) ifStatement() (statement.Stmt, error) {
	// Check if condition expression
	if _, err := p.consume(token.LEFT_PAREN, "Expected \"(\" after \"if\""); err != nil {
		return nil, err
	}
	condition, err := p.expression()
	if err != nil {
		return nil, err
	}
	if _, err := p.consume(token.RIGHT_PAREN, "Expected \")\" after if condition"); err != nil {
		return nil, err
	}

	// Check for then branch statement
	thenBranch, err := p.statement()
	if err != nil {
		return nil, err
	}

	// Check for else branch statement
	var elseBranch statement.Stmt
	if isElse, err := p.match(token.ELSE); isElse {
		elseBranch, err = p.statement()
		if err != nil {
			return nil, err
		}
	} else if err != nil {
		return nil, err
	}

	return &statement.If{
		Condition:  condition,
		ThenBranch: thenBranch,
		ElseBranch: elseBranch,
	}, nil
}

func (p *Parser) block() ([]statement.Stmt, error) {
	statements := []statement.Stmt{}

	for {
		isRightBrace, err := p.check(token.RIGHT_BRACE)
		if err != nil {
			return nil, err
		}

		atEnd, err := p.isAtEnd()
		if err != nil {
			return nil, err
		}

		if isRightBrace || atEnd {
			break
		}

		statement, err := p.declaration()
		if err != nil {
			return nil, err
		}

		statements = append(statements, statement)
	}

	_, err := p.consume(token.RIGHT_BRACE, "Expected \"}\" after block.")
	if err != nil {
		return nil, err
	}

	return statements, nil
}

func (p *Parser) printStatement() (statement.Stmt, error) {
	value, err := p.expression()
	if err != nil {
		return nil, err
	}

	_, err = p.consume(token.SEMICOLON, "Expected \";\" after expression.")
	if err != nil {
		return nil, err
	}

	return &statement.Print{Expression: value}, nil
}

func (p *Parser) exprStatement() (statement.Stmt, error) {
	exp, err := p.expression()
	if err != nil {
		return nil, err
	}

	_, err = p.consume(token.SEMICOLON, "Expected \";\" after expression.")
	if err != nil {
		return nil, err
	}

	return &statement.Expression{Expression: exp}, nil
}

func (p *Parser) expression() (expression.Expr, error) {
	return p.assignment()
}

func (p *Parser) assignment() (expression.Expr, error) {
	expr, err := p.or()
	if err != nil {
		return nil, err
	}

	// Check for COLON token for assignment
	isColon, err := p.match(token.COLON)
	if err != nil {
		return nil, err
	}

	if isColon {
		equals, err := p.previous()
		if err != nil {
			return nil, err
		}

		// Obtain r-value (a literal)
		value, err := p.equality()
		if err != nil {
			return nil, err
		}

		// Check if l-value is a variable
		variable, ok := expr.(*expression.Variable)
		if !ok {
			return nil, &exu_err.SyntaxError{
				Token:   equals,
				Message: "Invalid assignment target",
			}
		}

		return &expression.Assignment{
			Name:  variable.Name,
			Value: value,
		}, nil
	}

	return expr, nil
}

func (p *Parser) or() (expression.Expr, error) {
	expr, err := p.and()
	if err != nil {
		return nil, err
	}

	isOr, err := p.match(token.OR)
	if err != nil {
		return nil, err
	}

	for isOr {
		if err != nil {
			return nil, err
		}

		operator, err := p.previous()
		if err != nil {
			return nil, err
		}

		right, err := p.and()
		if err != nil {
			return nil, err
		}

		expr = &expression.Logical{
			Left:     expr,
			Operator: operator,
			Right:    right,
		}

		isOr, err = p.match(token.OR)
		if err != nil {
			return nil, err
		}
	}

	return expr, nil
}

func (p *Parser) and() (expression.Expr, error) {
	expr, err := p.equality()
	if err != nil {
		return nil, err
	}

	isAnd, err := p.match(token.AND)
	if err != nil {
		return nil, err
	}

	for isAnd {
		operator, err := p.previous()
		if err != nil {
			return nil, err
		}

		right, err := p.equality()
		if err != nil {
			return nil, err
		}

		expr = &expression.Logical{
			Left:     expr,
			Operator: operator,
			Right:    right,
		}

		// Update isAnd
		isAnd, err = p.match(token.AND)
		if err != nil {
			return nil, err
		}
	}

	return expr, nil
}

func (p *Parser) equality() (expression.Expr, error) {
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

		exp = &expression.Binary{Left: exp, Operator: operator, Right: right}

		// Update isEquality
		isEquality, err = p.match(token.EQUAL, token.NOT_EQUAL)
		if err != nil {
			return nil, err
		}
	}

	return exp, nil
}

func (p *Parser) comparison() (expression.Expr, error) {
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

		exp = &expression.Binary{Left: exp, Operator: operator, Right: right}

		// Update isComparison
		isComparison, err = p.match(token.GREATER, token.GREATER_EQUAL, token.LESS, token.LESS_EQUAL)
		if err != nil {
			return nil, err
		}
	}

	return exp, nil
}

func (p *Parser) term() (expression.Expr, error) {
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

		exp = &expression.Binary{Left: exp, Operator: operator, Right: right}

		// Update isTermOp
		isTermOp, err = p.match(token.MINUS, token.PLUS)
		if err != nil {
			return nil, err
		}
	}

	return exp, nil
}

func (p *Parser) factor() (expression.Expr, error) {
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

		exp = &expression.Binary{Left: exp, Operator: operator, Right: right}

		// Update isFactorOp
		isFactorOp, err = p.match(token.SLASH, token.STAR)
		if err != nil {
			return nil, err
		}
	}

	return exp, nil
}

func (p *Parser) unary() (expression.Expr, error) {
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

		return &expression.Unary{Operator: operator, Right: right}, nil
	}

	return p.call()
}

func (p *Parser) call() (expression.Expr, error) {
	expr, err := p.primary()
	if err != nil {
		return nil, err
	}

	// Check for potential call arguments
	for {
		if isLeftParen, err := p.match(token.LEFT_PAREN); err != nil {
			return nil, err
		} else if isLeftParen {
			// Parse call arguments
			expr, err = p.finishCall(expr)
			if err != nil {
				return nil, err
			}
		} else {
			break
		}
	}

	return expr, nil
}

func (p *Parser) primary() (expression.Expr, error) {
	// Check for FALSE token
	isFalse, err := p.match(token.FALSE)
	if err != nil {
		return nil, err
	}
	if isFalse {
		return &expression.Literal{Value: func() *literal.BoolLiteral {
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
		return &expression.Literal{Value: func() *literal.BoolLiteral {
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
		return &expression.Literal{Value: &literal.NullLiteral{}}, nil
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
		return &expression.Literal{Value: prev.Literal}, nil
	}

	// Check for variables
	isIdentifier, err := p.match(token.IDENTIFIER)
	if err != nil {
		return nil, err
	}
	if isIdentifier {
		prev, err := p.previous()
		if err != nil {
			return nil, err
		}

		// Return variable expression from identifier token
		return &expression.Variable{Name: prev}, nil
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
		_, err = p.consume(token.RIGHT_PAREN, "Expected ')' after expression")
		if err != nil {
			return nil, err
		}

		return &expression.Grouping{Expression: exp}, nil
	}

	// Throws an error on the current token
	// if it doesnt match with any of the
	// above conditions
	currToken, err := p.peek()
	if err != nil {
		return nil, err
	}

	return nil, &exu_err.SyntaxError{
		Token:   currToken,
		Message: fmt.Sprintf("Expected expression but got %v", currToken.Lexeme),
	}
}

// HELPER FUNCTIONS

// Parses call arguments
func (p *Parser) finishCall(callee expression.Expr) (expression.Expr, error) {
	arguments := []expression.Expr{}

	if isRightParen, err := p.check(token.RIGHT_PAREN); err != nil {
		return nil, err
	} else if !isRightParen {
		// Parse arguments into a list
		// Execute loop first before evaluating condition
		// (stored in isComma)
		isComma := true

		for isComma {
			// If no. of arguments exceed 255, display an error
			// but continue parsing the expression
			// Treat this like displaying a warning
			if len(arguments) >= 255 {
				currToken, err := p.peek()
				if err != nil {
					return nil, err
				}

				return nil, &exu_err.SyntaxError{
					Token:   currToken,
					Message: "A call cannot have more than 255 arguments",
				}
			}

			argument, err := p.expression()
			if err != nil {
				return nil, err
			}

			arguments = append(arguments, argument)

			isComma, err = p.match(token.COMMA)
			if err != nil {
				return nil, err
			}
		}
	}

	paren, err := p.consume(token.RIGHT_PAREN, "Expected \")\" after arguments")
	if err != nil {
		return nil, err
	}

	return &expression.Call{
		Callee:    callee,
		Paren:     paren,
		Arguments: arguments,
	}, nil
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

	currToken, err := p.peek()
	if err != nil {
		return token.Token{}, err
	}

	// Throws an error if there is a type mismatch
	return token.Token{}, &exu_err.SyntaxError{Token: currToken, Message: errorMsg}
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
		p.Curr++
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
		return token.Token{}, &exu_err.ParseError{
			Curr:    p.Curr,
			Message: "Index out of bounds",
		}
	}
	return p.Tokens[p.Curr], nil
}

// Gets previous token
func (p *Parser) previous() (token.Token, error) {
	// We shift the bounds one unit to the right
	// since we are accessing the previous token
	if p.Curr < 1 || p.Curr >= len(p.Tokens)+1 {
		return token.Token{}, &exu_err.ParseError{
			Curr:    p.Curr - 1,
			Message: "Index out of bounds",
		}
	}
	return p.Tokens[p.Curr-1], nil
}

// Exits expression tree and proceed to next statement labelled by a keyword
func (p *Parser) synchronize() error {
	keywords := []token.TokenType{
		token.CLASS, token.FN, token.LET, token.FOR, token.IF,
		token.WHILE, token.PRINT, token.RETURN,
	}

	_, err := p.advance()
	if err != nil {
		return err
	}

	isEOF, err := p.isAtEnd()
	if err != nil {
		return err
	}

	for isEOF {
		prev, err := p.previous()
		if err != nil {
			return err
		}

		if prev.TokenType == token.SEMICOLON {
			return nil
		}

		currToken, err := p.peek()
		if err != nil {
			return err
		}

		if slices.Contains(keywords, currToken.TokenType) {
			return nil
		}

		_, err = p.advance()
		if err != nil {
			return err
		}

		// Update isEOF
		isEOF, err = p.isAtEnd()
		if err != nil {
			return err
		}
	}

	return nil
}

// Utility function to check if curr is a
// valid index in tokens
func (p *Parser) isCurrInBounds() bool {
	if p.Curr < 0 || p.Curr >= len(p.Tokens) {
		return false
	}
	return true
}
