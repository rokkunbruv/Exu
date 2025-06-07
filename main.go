package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "Hello 世界! 🌍 π ★ ABC123"

	for i, v := range str {
		_, width := utf8.DecodeRuneInString(str[i:])
		n, _ := utf8.DecodeRuneInString(str[i+width:])
		fmt.Printf("Index: %v, Character: %v, Next Index: %v, Next Character: %v\n", i, string(v), i+width, string(n))
	}
}
