# Exu Syntax and Semantics

This document defines the syntax and semantics of the Exu programming language.

## Basic Overview

Exu is a statically-typed high-level programming language.

## Data Types

### Data Primitives

Exu supports the following data primitives:

- **Numerics (`num`)** - a 64-bit floating point number.
- **Strings (`str`)** - an array of ASCII characters.
- **Booleans (`bool`)** - a value evaluating to true/false.

### Compound Types

Exu supports the following compound datatypes:

- **Functions (`fn`)** - a type that returns a value given some arguments as inputs.
- **Entities (`ent`)** - essentially C-like structs.
  - **Instances** - an instance of an entity. Their type is defined by their respective entity name.

### Collective Types

Exu supports the following collective types:

- **Arrays (`arr`)** - arrays with a fixed size
- **Vectors (`vec`)** - dynamically-sized arrays
- **Lists (`list`)** - linked lists

## Expressions

Expressions in Exu are categorized into arithmetic, comparison, and logical, each with their own designated operators. These operators can be used together in a single expression.

### Arithmetic Expressions

Exu supports the following arithmetic operations:

- **Addition** with the `+` operator.
- **Subtraction** with the `-` operator.
- **Multiplication** with the `*` operator.
- **Division** with the `/` operator.

### Comparison Expressions

Exu support the following comparison operations:

- **Equality** with the `==` operator.
- **Non-equality** with the `!=` operator.
- **Greater than** with the `>` operator.
- **Greater than or equal** with the `>=` operator.
- **Less than** with the `<` operator.
- **Less than or equal** with the `<=` operator.

### Logical Expressions

Exu support the following logical operations:

- **Logical and** with the `&` operator.
- **Logical or** with the `|` operator.
- **Logical not** with the `~` operator.

## Variable Declarations

Variable declarations are invoked by the following syntax:

```exu
let <type> <var_name> (: <initializer>)? ;
```

Exu does not support multiple and consecutive variable declarations in a single declaration statement.

A variable can only be declared once.

## Variable Assignments

An existing variable can be reassigned using the `:` operator.

```exu
<var_name> : <new_val> ;
```

Variable assignments are treated as statements in Exu. The interpreter throws an error if the assignment value does not match with the variable's datatype

## Procedure Declarations

Procedures are declared by the following syntax:

```exu
proc <proc_name> '(' (<type> <param_name> ,)* ')' { (<statements>)* }
```

Procedures are always declared at the global scope. This means that they cannot be declared in local scopes (i.e. inside blocks or procedures). Furthermore, procedures are not first-class.

### Anons
