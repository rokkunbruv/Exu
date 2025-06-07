package lexer

import (
	"fmt"
	"testing"
	"unicode/utf8"

	"github.com/attic-labs/testify/assert"
)

func TestPeek(t *testing.T) {
	type testCase struct {
		source string
		curr   int

		expected rune
		err      error
	}

	t.Run("Lexer", func(t *testing.T) {
		tests := []testCase{
			{
				source:   "hello",
				curr:     0,
				expected: 'e', // Peek should return next character
			},
			{
				source:   "hello",
				curr:     4,
				expected: 0, // At end of string
			},
			{
				source:   "hello",
				curr:     5,
				expected: 0, // Beyond string length
			},
			{
				source:   "你好", // Chinese characters
				curr:     0,
				expected: '好', // Peek next Chinese character
			},
			{
				source:   "你好",
				curr:     1,              // Middle of UTF-8 bytes
				expected: utf8.RuneError, // Invalid position
			},
			{
				source:   "🌍world", // Emoji (4 bytes) + ascii
				curr:     0,
				expected: 'w', // Peek should return 'w' after emoji
			},
			{
				source:   "🌍world",
				curr:     utf8.RuneError, // Middle of emoji bytes
				expected: 0,              // Invalid position
			},
			{
				source:   "π = 3.14",
				curr:     0,
				expected: ' ', // Peek space after π
			},
			{
				source:   "",
				curr:     0,
				expected: 0, // Empty string
			},
			{
				source:   "hello世界",
				curr:     4,
				expected: '世', // Peek Chinese character after ASCII
			},
		}

		for i, test := range tests {
			actual := peek(test.source, test.curr)
			assert.Equal(t, test.expected, actual, fmt.Sprintf("ASSERT EQUAL ERROR on Test Case #%v: Expected %v does not match with actual %v on source = %v and curr = %v", i, test.expected, actual, test.source, test.curr))

		}
	})
}
