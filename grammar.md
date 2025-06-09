# Exu Grammar

## Expressions

Gramar Productions in Order from Lowest to Highest Precedence
(unary > mul/div > add/sub > inequality > equality)

```txt
expression -> equality
equality   -> comparison (( "!=" | "=" ) comparison)*
comparison -> term (( ">" | ">=" | "<" | "<=" ) term)*
term       -> factor (( "+" | "-" ) factor)*
factor     -> unary (( "*" | "/" ) unary)*
unary      -> ( "!" | "-" ) unary | primary
primary    -> NUMBER | STRING | "true" | "false" | "null" | "(" expression ")"
```
