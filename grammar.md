# Exu Grammar

The grammar and syntax specifications of Exu closely resembles Lox given that Exu is based on Lox.

## Grammar Definition

Full Exu Grammar Productions Definition

```txt
program     -> declaration* EOF
declaration -> varDecl | statement
varDecl     -> "let" IDENTIFIER (":" expression)? ";"
statement   -> exprStmt | printStmt | ifStmt | whileStmt | forStmt | block
exprStmt    -> expression ";"
printStmt   -> "print" expression ";"
whileStmt   -> "while" "(" expression ")" statement
forStmt     -> "for" "(" (varDecl | exprStmt | ";") expression? ";" expression? ")" statement
ifStmt      -> "if" "(" expression ")" statement ("else" statement)?
block       -> "{" declaration* "}"
expression  -> assignment
assignment  -> IDENTIFIER ":" logic_or | logic_or
logic_or    -> logic_and ("or" logic_and)*
logic_and   -> equality ("and" equality)*
equality    -> comparison (( "!=" | "=" ) comparison)*
comparison  -> term (( ">" | ">=" | "<" | "<=" ) term)*
term        -> factor (( "+" | "-" ) factor)*
factor      -> unary (( "*" | "/" ) unary)*
unary       -> ( "!" | "-" ) unary | primary
primary     -> NUMBER | STRING | "true" | "false" | "null" | "(" expression ")" | IDENTIFIER
```

## Grammar Specifications

### Expressions

Expressions in Exu perform operations between literals. An Exu expression involves a `primary` and an `operator`. A `primary` is a nonterminal
that represents Exu literals. Exu literals can either be raw literals or results from evaluated expressions. An `operator` denotes the operation
to be performed on a set of literals or expressions. Exu supports chained operations as defined from the grammar and are considered valid expressions.

The literals themselves take the highest precedence and thus will be evaluated first. This includes grouped expressions enclosed in parenthesis as
defined by the grammar. The following operators, or operations, will follow the precendence hierarchy in descending order, from highest to lowest precedence:

- Unary operations (logical and arithmetic negation)
- Multiplication and division operations
- Addition and subtraction operations
- Comparison operations
- Equality operations

As observed closely from the grammar, expressions also accept identifiers as literals since identifiers are represented as variables in Exu,
which can hold a literal value.

### Statements

Statements in Exu executes or evaluates operations. Statements always end with a semicolon to signify the end of a statement.
Exu evaluates the following statements:

- **Expression** statements evaluates an expression. The literals resulted from the evaluated expressions can either be stored in a variable or left alone.
- **Print** statements displays the result of the evaluated expression in the terminal. Unlike most other languages that uses library functions to perform
  output operations, Exu has a designated keyword `print` to perform an immediate and simple output operation.

### Declarations

Declarations in Exu initializes Exu objects.

- **Variables** are initialized using the `let` keyword. Variables can store a literal value.

### Scope

Local scopes are established using `block` statements. Block statements can hold statements and declarations.
