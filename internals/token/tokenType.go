package token

type TokenType string

const (
	LEFT_PAREN  TokenType = "LEFT_PAREN"
	RIGHT_PAREN TokenType = "RIGHT_PAREN"
	LEFT_BRACE  TokenType = "LEFT_BRACE"
	RIGHT_BRACE TokenType = "RIGHT_BRACE"
	SEMICOLON   TokenType = "SEMICOLON"
	COLON       TokenType = "COLON"
	COMMA       TokenType = "COMMA"
	DOT         TokenType = "DOT"

	// Operators
	PLUS          TokenType = "PLUS"
	MINUS         TokenType = "MINUS"
	STAR          TokenType = "STAR"
	SLASH         TokenType = "SLASH"
	EQUAL         TokenType = "EQUAL"
	NOT           TokenType = "NOT"
	NOT_EQUAL     TokenType = "NOT_EQUAL"
	GREATER       TokenType = "GREATER"
	GREATER_EQUAL TokenType = "GREATER_EQUAL"
	LESS          TokenType = "LESS"
	LESS_EQUAL    TokenType = "LESS_EQUAL"
	LEFT_ARROW    TokenType = "LEFT_ARROW"
	AND           TokenType = "AND"
	OR            TokenType = "OR"

	// Literals
	STRING     TokenType = "STRING"
	NUMERIC    TokenType = "NUMERIC"
	IDENTIFIER TokenType = "IDENTIFIER" // Variable names

	// Keywords
	TRUE   TokenType = "TRUE"
	FALSE  TokenType = "FALSE"
	NULL   TokenType = "NULL"
	IF     TokenType = "IF"
	ELSE   TokenType = "ELSE"
	FOR    TokenType = "FOR"
	WHILE  TokenType = "WHILE"
	FN     TokenType = "FN"
	RETURN TokenType = "RETURN"
	PRINT  TokenType = "PRINT"
	CLASS  TokenType = "CLASS"
	SUPER  TokenType = "SUPER"
	SELF   TokenType = "SELF"

	EOF TokenType = "EOF"
)
