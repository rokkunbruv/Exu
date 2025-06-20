package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/rokkunbruv/internals/interpreter"
	"github.com/rokkunbruv/internals/lexer"
	"github.com/rokkunbruv/internals/parser"
)

func main() {
	switch {
	case len(os.Args) > 2:
		fmt.Println("Correct Usage: exu [script]")
		os.Exit(1)
	case len(os.Args) == 2:
		runFile(os.Args[1])
	default:
		runPrompt()
	}
}

// Executes the source code if a script path is provided
func runFile(path string) {
	if !strings.HasSuffix(path, ".exu") {
		fmt.Println("File error: File type not supported. Use \".exu\"")
		return
	}

	f, err := os.ReadFile(path)
	if err != nil {
		fmt.Println("File error: File cannot be read (%w)", err)
		return
	}

	fStr := string(f)

	run(fStr)
}

// Executes the REPL when called
func runPrompt() {
	linePrefix := ">> "

	fmt.Println("Exu REPL Version 0.1.0")

	reader := bufio.NewScanner(os.Stdin)

	fmt.Print(linePrefix)
	for reader.Scan() {
		line := cleanLine(reader.Text())
		run(line)
		fmt.Print(linePrefix)
	}
}

func run(line string) {
	// Tokenize code
	tokens, err := lexer.Lexer(line)
	if err != nil {
		// error handling
		fmt.Println(err)
		return
	}

	// Parse tokens to an AST
	p := parser.Parser{Tokens: tokens, Curr: 0}
	statements, err := p.Parse()
	if err != nil {
		fmt.Println(err)
		return
	}

	// Evaluate AST
	intr := interpreter.Interpreter{Statements: statements}
	err = intr.Interpret()
	if err != nil {
		fmt.Println(err)
		return
	}
}

func cleanLine(line string) string {
	output := strings.TrimSpace(line)
	output = strings.ToLower(output)
	return output
}
