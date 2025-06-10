package literal

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLiteralToString(t *testing.T) {
	tests := []struct {
		name     string
		literal  Literal
		expected string
	}{
		{
			name: "test valid string literal",
			literal: func() *StringLiteral {
				lit := &StringLiteral{}
				lit.SetVal("hello")
				return lit
			}(),
			expected: "hello",
		},
		{
			name: "test valid numeric literal",
			literal: func() *NumericLiteral {
				lit := &NumericLiteral{}
				lit.SetVal(42.5)
				return lit
			}(),
			expected: "42.5",
		},
		{
			name: "test valid bool literal (true)",
			literal: func() *BoolLiteral {
				lit := &BoolLiteral{}
				lit.SetVal(true)
				return lit
			}(),
			expected: "true",
		},
		{
			name: "test valid bool literal (false)",
			literal: func() *BoolLiteral {
				lit := &BoolLiteral{}
				lit.SetVal(false)
				return lit
			}(),
			expected: "false",
		},
		{
			name: "test null string literal",
			literal: &StringLiteral{
				IsNull: true,
			},
			expected: "",
		},
		{
			name: "test null numeric literal",
			literal: &NumericLiteral{
				IsNull: true,
			},
			expected: "",
		},
		{
			name: "test null bool literal",
			literal: &BoolLiteral{
				IsNull: true,
			},
			expected: "",
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result := test.literal.ToString()
			assert.Equal(t, test.expected, result)
		})
	}
}
