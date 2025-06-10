package literal

import (
	"fmt"
)

// Literal interface for literal types
// Literal structs have IsNull field (true if null)
type Literal interface {
	ToString() string
}

// Numeric Literal
type NumericLiteral struct {
	val    float64
	IsNull bool
}

func (n *NumericLiteral) ToString() string {
	if (*n).IsNull {
		return ""
	}
	return fmt.Sprint((*n).val)
}

func (n *NumericLiteral) Val() (float64, error) {
	if (*n).IsNull {
		return 0, fmt.Errorf("invalid access to a null numeric literal")
	}
	return (*n).val, nil
}

func (n *NumericLiteral) SetVal(newVal float64) {
	if (*n).IsNull {
		(*n).IsNull = false
	}
	(*n).val = newVal
}

// String Literal
type StringLiteral struct {
	val    string
	IsNull bool
}

func (s *StringLiteral) ToString() string {
	if (*s).IsNull {
		return ""
	}
	return (*s).val
}

func (s *StringLiteral) Val() (string, error) {
	if (*s).IsNull {
		return "", fmt.Errorf("invalid access to a null string literal")
	}
	return (*s).val, nil
}

func (s *StringLiteral) SetVal(newVal string) {
	if (*s).IsNull {
		(*s).IsNull = false
	}
	(*s).val = newVal
}

// Bool Literal
type BoolLiteral struct {
	val    bool
	IsNull bool
}

func (b *BoolLiteral) ToString() string {
	if (*b).IsNull {
		return ""
	}
	if (*b).val {
		return "true"
	}
	return "false"
}

func (b *BoolLiteral) Val() (bool, error) {
	if (*b).IsNull {
		return false, fmt.Errorf("invalid access to a null bool literal")
	}
	return (*b).val, nil
}

func (b *BoolLiteral) SetVal(newVal bool) {
	if (*b).IsNull {
		(*b).IsNull = false
	}
	(*b).val = newVal
}
