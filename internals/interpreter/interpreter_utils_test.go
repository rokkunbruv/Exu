package interpreter

import (
	"fmt"
	"testing"

	"github.com/rokkunbruv/internals/literal"
	"github.com/stretchr/testify/assert"
)

func TestIsEqual(t *testing.T) {
	type testCase struct {
		name string

		lit1 literal.Literal
		lit2 literal.Literal

		expected bool
		err      error
	}

	tests := []testCase{
		{
			name:     "test two equal numeric literals",
			lit1:     literal.GenerateNumericLiteral(0),
			lit2:     literal.GenerateNumericLiteral(0),
			expected: true,
			err:      nil,
		},
		{
			name:     "test two non equal numeric literals",
			lit1:     literal.GenerateNumericLiteral(0),
			lit2:     literal.GenerateNumericLiteral(1),
			expected: false,
			err:      nil,
		},
		{
			name:     "test two equal bool literals",
			lit1:     literal.GenerateBoolLiteral(true),
			lit2:     literal.GenerateBoolLiteral(true),
			expected: true,
			err:      nil,
		},
		{
			name:     "test two non equal bool literals",
			lit1:     literal.GenerateBoolLiteral(true),
			lit2:     literal.GenerateBoolLiteral(false),
			expected: false,
			err:      nil,
		},
		{
			name:     "test two equal string literals",
			lit1:     literal.GenerateStringLiteral("str"),
			lit2:     literal.GenerateStringLiteral("str"),
			expected: true,
			err:      nil,
		},
		{
			name:     "test two non equal string literals",
			lit1:     literal.GenerateStringLiteral("str"),
			lit2:     literal.GenerateStringLiteral("st"),
			expected: false,
			err:      nil,
		},
		{
			name:     "test lit1 null literal",
			lit1:     &literal.NullLiteral{},
			lit2:     literal.GenerateNumericLiteral(0),
			expected: false,
			err:      nil,
		},
		{
			name:     "test lit2 null literal",
			lit1:     literal.GenerateNumericLiteral(0),
			lit2:     &literal.NullLiteral{},
			expected: false,
			err:      nil,
		},
		{
			name:     "test two null literals",
			lit1:     &literal.NullLiteral{},
			lit2:     &literal.NullLiteral{},
			expected: true,
			err:      nil,
		},
		{
			name:     "test two literals of different types",
			lit1:     literal.GenerateNumericLiteral(0),
			lit2:     literal.GenerateStringLiteral("str"),
			expected: false,
			err:      fmt.Errorf(""),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := isEqual(test.lit1, test.lit2)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestToNumeric(t *testing.T) {
	type testCase struct {
		name string

		lit literal.Literal

		expected float64
		err      error
	}

	tests := []testCase{
		{
			name:     "test numeric literal",
			lit:      literal.GenerateNumericLiteral(0),
			expected: 0,
			err:      nil,
		},
		{
			name:     "test string literal",
			lit:      literal.GenerateStringLiteral("str"),
			expected: 0,
			err:      fmt.Errorf(""),
		},
		{
			name:     "test bool literal",
			lit:      literal.GenerateBoolLiteral(false),
			expected: 0,
			err:      fmt.Errorf(""),
		},
		{
			name:     "test null literal",
			lit:      &literal.NullLiteral{},
			expected: 0,
			err:      fmt.Errorf(""),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := toNumeric(test.lit)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestToTwoNumerics(t *testing.T) {
	type testCase struct {
		name string

		lit1 literal.Literal
		lit2 literal.Literal

		expected1 float64
		expected2 float64
		err       error
	}

	tests := []testCase{
		{
			name:      "test two numeric literals",
			lit1:      literal.GenerateNumericLiteral(0),
			lit2:      literal.GenerateNumericLiteral(1),
			expected1: 0,
			expected2: 1,
			err:       nil,
		},
		{
			name:      "test lit1 non numeric literal",
			lit1:      literal.GenerateStringLiteral("str"),
			lit2:      literal.GenerateNumericLiteral(0),
			expected1: 0,
			expected2: 0,
			err:       fmt.Errorf(""),
		},
		{
			name:      "test lit2 non numeric literal",
			lit1:      literal.GenerateNumericLiteral(0),
			lit2:      literal.GenerateBoolLiteral(false),
			expected1: 0,
			expected2: 0,
			err:       fmt.Errorf(""),
		},
		{
			name:      "test two non numeric literals",
			lit1:      &literal.NullLiteral{},
			lit2:      literal.GenerateBoolLiteral(false),
			expected1: 0,
			expected2: 0,
			err:       fmt.Errorf(""),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual1, actual2, err := toTwoNumerics(test.lit1, test.lit2)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected1, actual1)
			assert.Equal(t, test.expected2, actual2)
		})
	}
}

func TestToStr(t *testing.T) {
	type testCase struct {
		name string

		lit literal.Literal

		expected string
		err      error
	}

	tests := []testCase{
		{
			name:     "test string literal",
			lit:      literal.GenerateStringLiteral("str"),
			expected: "str",
			err:      nil,
		},
		{
			name:     "test empty string literal",
			lit:      literal.GenerateStringLiteral(""),
			expected: "",
			err:      nil,
		},
		{
			name:     "test bool literal",
			lit:      literal.GenerateBoolLiteral(true),
			expected: "",
			err:      fmt.Errorf(""),
		},
		{
			name:     "test numeric literal",
			lit:      literal.GenerateNumericLiteral(0),
			expected: "",
			err:      fmt.Errorf(""),
		},
		{
			name:     "test null literal",
			lit:      &literal.NullLiteral{},
			expected: "",
			err:      fmt.Errorf(""),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := toStr(test.lit)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestToTwoStrs(t *testing.T) {
	type testCase struct {
		name string

		lit1 literal.Literal
		lit2 literal.Literal

		expected1 string
		expected2 string
		err       error
	}

	tests := []testCase{
		{
			name:      "test two string literals",
			lit1:      literal.GenerateStringLiteral("str1"),
			lit2:      literal.GenerateStringLiteral("str2"),
			expected1: "str1",
			expected2: "str2",
			err:       nil,
		},
		{
			name:      "test lit1 non string literal",
			lit1:      literal.GenerateNumericLiteral(0),
			lit2:      literal.GenerateStringLiteral("str"),
			expected1: "",
			expected2: "",
			err:       fmt.Errorf(""),
		},
		{
			name:      "test lit2 non string literal",
			lit1:      literal.GenerateStringLiteral("str"),
			lit2:      literal.GenerateBoolLiteral(false),
			expected1: "",
			expected2: "",
			err:       fmt.Errorf(""),
		},
		{
			name:      "test two non string literals",
			lit1:      &literal.NullLiteral{},
			lit2:      literal.GenerateBoolLiteral(false),
			expected1: "",
			expected2: "",
			err:       fmt.Errorf(""),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual1, actual2, err := toTwoStrs(test.lit1, test.lit2)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected1, actual1)
			assert.Equal(t, test.expected2, actual2)
		})
	}
}

func TestToBool(t *testing.T) {
	type testCase struct {
		name string

		lit literal.Literal

		expected bool
		err      error
	}

	tests := []testCase{
		{
			name:     "test bool literal (true)",
			lit:      literal.GenerateBoolLiteral(true),
			expected: true,
			err:      nil,
		},
		{
			name:     "test bool literal (false)",
			lit:      literal.GenerateBoolLiteral(false),
			expected: false,
			err:      nil,
		},
		{
			name:     "test string literal",
			lit:      literal.GenerateStringLiteral("str"),
			expected: false,
			err:      fmt.Errorf(""),
		},
		{
			name:     "test numeric literal",
			lit:      literal.GenerateNumericLiteral(0),
			expected: false,
			err:      fmt.Errorf(""),
		},
		{
			name:     "test null literal",
			lit:      &literal.NullLiteral{},
			expected: false,
			err:      fmt.Errorf(""),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual, err := toBool(test.lit)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected, actual)
		})
	}
}

func TestToTwoBools(t *testing.T) {
	type testCase struct {
		name string

		lit1 literal.Literal
		lit2 literal.Literal

		expected1 bool
		expected2 bool
		err       error
	}

	tests := []testCase{
		{
			name:      "test two bool literals",
			lit1:      literal.GenerateBoolLiteral(true),
			lit2:      literal.GenerateBoolLiteral(false),
			expected1: true,
			expected2: false,
			err:       nil,
		},
		{
			name:      "test lit1 non bool literal",
			lit1:      literal.GenerateStringLiteral("str"),
			lit2:      literal.GenerateBoolLiteral(true),
			expected1: false,
			expected2: false,
			err:       fmt.Errorf(""),
		},
		{
			name:      "test lit2 non bool literal",
			lit1:      literal.GenerateBoolLiteral(false),
			lit2:      literal.GenerateNumericLiteral(0),
			expected1: false,
			expected2: false,
			err:       fmt.Errorf(""),
		},
		{
			name:      "test two non bool literals",
			lit1:      &literal.NullLiteral{},
			lit2:      literal.GenerateStringLiteral("false"),
			expected1: false,
			expected2: false,
			err:       fmt.Errorf(""),
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			actual1, actual2, err := toTwoBools(test.lit1, test.lit2)
			if test.err != nil || err != nil {
				assert.EqualError(t, err, test.err.Error())
			}
			assert.Equal(t, test.expected1, actual1)
			assert.Equal(t, test.expected2, actual2)
		})
	}
}
