# Exu Syntax Design

## Preface

The **Exu** programming language is based on the Lox programming language developed in Crafting Interpreters by Robert Nymstrom

## Grammar

### Data Types

Exu supports the following primitive data types:

- **Numbers** (32-bit floating point numbers)
- **Strings**
- **Booleans**
- **Null** (denoted as `null`)

### Variables

Variables in Exu are dynamically-typed. Variable naming follows standard naming conventions, which are:

- Variable names can only contain alphanumeric and underscore characters
- Variable names cannot start with a numeric character

Values are assigned to values by the `:` operator

```exu
name: value
```

Unassigned variables are automatically assigned to `null` by the interpreter.

```exu
name
```

`name` here has an implicit value of `null`.

### Expressions

Expressions in Exu are categorized by the ff:

#### Arithmetic

Arithmetic expressions support the ff. operators:

- `+` for addition
- `-` for subtraction and negation
- `*` for multiplication
- `/` for division

Exu does not support arithmetic operations for non-numeric data types. The
only exception is the `+` operator on strings to perform concatenation.

#### Comparison

Comparison expressions support the ff. operators:

- `<` denotes less than
- `<=` denotes less than or equal
- `>` denotes greater than
- `>=` denotes greather than or equal
- `=` denotes is equal to
- `!=` denotes is not equal to

Comparison operators can be performed on two values of different data types.

#### Logical

Logical expressions support the ff. operators:

- `&` denotes logical and
- `|` denotes logical or
- `!` denotes logical not

Expressions in Exu can be grouped by parentheses `()`

```exu
var: (12 + 18.5) != "hello world"
```

### Statements

The `;` operator marks the end of a statement in Exu.

```exu
var: 12;
```

### Blocks

Blocks in Exu are defined by curly brackets `{}`.

### Comments

Comments are always preceded by the `#` operator.

```exu
# This is a comment
var: 12;
```

### Input/Output

As of now, Exu does not support input operations. However, Exu supports one output operation, printing to the terminal. The `print` keyword can be used to print any value besides it.

```exu
print "Hello World"; # outputs "Hello World" on the terminal

var: 5;
print var; # outputs "5" on the terminal

fn function() {
    # some code
}
print function # outputs "null"
```

### Control Flow

#### If-Else Statements

A typical if-else statement in Exu looks like this:

```exu
if (condition) {
    # execute if block
} else {
    # execute else block
}
```

#### While Loops

A typical while loop in Exu looks like this:

```exu
while (condition) {
    # execute loop
}
```

#### For Loops

A typical for loop in Exu looks like this:

```exu
for (initializer; condition; iterator) {
    # execute loop
}
```

To make things more clearer, an implementation of a for loop would look something
like this:

```exu
for (i: 0; i <= 5; i: i + 1) {
    # execute loop
}
```

### Functions

Functions are defined by the `fn` keyword.

```exu
fn function(parameter1, parameter2) {
    # function definition
}
```

Functions are called just like how functions are typically called in most
programming languages.

```exu
function(12, "hello world");
```

Functions in Exu can also return a value using the `return` keyword

```exu
fn function(parameter1, paramter2) {
    return parameter1 + parameter2
}

var: function(12, 15); # var: 27
```

If a function does not return a value, it implicitly returns `null`

```exu
fn function() {
    # execute some code
}

var: function(); # var: null
```

### Higher Order Functions

Exu treats functions as first class, and thus supports higher order functions. This means the ff:

- Functions can be defined within functions (nested functions)

    ```exu
    fn parent() {
        fn child() {
            # some code
        }

        child();
    }
    ```

- Functions are treated as values. This means that functions can be passed to
other functions as arguments or can be returned by parent functions.

    ```exu
    fn func1(var1, var2) {
        return var1 + var2;
    }

    fn func2(var) {
        return var;
    }

    func2(func1)(1, 2);
    ```

    ```exu
    fn parent() {
        fn child() {
            # some code
        }

        return child;
    }
    ```

    As a consequence of the code above, any variable can act as a function that
    executes code.

    ```exu
    var: parent(); # executes child()
    ```

### Classes

Classes are defined in Exu by the `class` keyword.

```exu
class MyClass {
    # some code
}
```

Classes are also considered first-class by Exu. This means it can be passed to as arguments to functions and returned by functions

```exu
fn func(var) {
    return MyClass;
}

func(MyClass);
```

We can create instances of our classes by invoking the factory method of a class.

```Exu
class MyClass {
    # some code
}

# var is an instance of MyClass
# instantiated by MyClass's factory method [MyClass()]
var: MyClass() 
```

Classes can either contain attributes or methods.

```exu
class MyClass {
    # This is an attribute
    attribute;

    # This is a method
    method() {
        # some code
    }
}
```

Class attributes and methods can be accessed by the `.` operator.

```exu
var1: MyClass()
var2: var1.attribute
var3: var1.method
```

Class attributes and methods can be accessed within the class definition by the `self` keyword.

```exu
class MyClass {
    attribute;

    method1() {
        return self.attribute;
    }

    method2() {
        self.method1();
    }
}
```

Class constructors can be created using the `init` keyword.

```exu
class MyClass {
    init(parameter1, parameter2) {
        # some code
    }
}

var: MyClass(12, "hello world");
```

Classes can be inherited from other classes using the `<-` operator.

```exu
class ParentClass {
    # some code
}

# ChildClass is inherited from ParentClass
class ChildClass <- ParentClass {
    # some code
}
```

If we want to access attributes or methods from the parent class, typically because they are being overriden by the child class, we can use the `super` keyword to indicate that an attribute or method is being accessed directly from the parent class.

```exu
class ParentClass {
    init() {
        # some code
    }
}

class ChildClass <- ParentClass {
    init() {
        super.init(); # This invokes the constructor of the parent class
    }
}
```
