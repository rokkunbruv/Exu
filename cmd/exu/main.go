package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"github.com/rokkunbruv/internals/ast"
	"github.com/rokkunbruv/internals/lexer"
	"github.com/rokkunbruv/internals/parser"
)

func main() {
	var hadError bool

	switch {
	case len(os.Args) > 2:
		fmt.Println("Correct Usage: exu [script]")
		os.Exit(1)
	case len(os.Args) == 2:
		runFile(os.Args[1], &hadError)
	default:
		runPrompt(&hadError)
	}
}

// Executes the source code if a script path is provided
func runFile(path string, hadError *bool) {

}

// Executes the REPL when called
func runPrompt(hadError *bool) {
	fmt.Println("Exu REPL Version 0.1.0")

	reader := bufio.NewScanner(os.Stdin)

	fmt.Print(">> ")
	for reader.Scan() {
		line := cleanLine(reader.Text())
		run(line)
		*hadError = false
		fmt.Print(">> ")
	}
}

func run(line string) {
	// Tokenize code
	tokens, err := lexer.Lexer(line)
	if err != nil {
		// error handling
		fmt.Println(err.Error())
	}

	exp, err := parser.Parse(tokens)
	if err != nil {
		fmt.Println(err.Error())
	}

	expStr, err := ast.ToString(exp)
	if err != nil {
		fmt.Println(err.Error())
	}
	fmt.Println(expStr)
}

func cleanLine(line string) string {
	output := strings.TrimSpace(line)
	output = strings.ToLower(output)
	return output
}
