# Libra ♎️

Libra is a basic stack-based programming language simulated through Haskell.
Heavily inspired by [Porth](https://github.com/tsoding/porth) and [Forth](<https://en.wikipedia.org/wiki/Forth_(programming_language)>)

Goals:
- [x] Basic integer operations
- [x] Data types "String" and "Boolean"
- [x] While loops
- [x] If statements
- [ ] Function macros
- [ ] Simple memory
- [ ] Turing completeness

## Example
Print numbers and words:
```pascal
123 456 + print
"Hello World!" print
```
yields
```
579
Hello World!
```
Print even numbers between 0 and 10
```pascal
10 0 while 2dup < run
  "Number: " put dup print
  2 +
end
```
yields
```
Number: 0
Number: 2
Number: 4
Number: 6
Number: 8
```

## Usage

Libra programs are simulated in Haskell, best done through the interactive shell started by running
```bash
$ ghci run.hs
```
Run programs by running
```bash
λ> run "file.♎️"
```

## Language

### Data types

Currently supported data types are integers, strings, and booleans. Pushed onto the stack as follows:
```pascal
123 "Hello World!" False
```

### Control flow

#### If statements

`if <body> end`

If checks if the top stack element is True, if so it executes the body, else it skips to end.

#### While loops

`while <cond> run <body> end`

While checks if a certain condition is true, if so it runs the body and returns to the while, if not, it skips to end.

### Manipulation words

#### Integer arithmetic

- `+` : Sum the two top stack elements
```pascal
1 2 + print
```
yields `3`

- `-` : Subtract the two top stack elements
```pascal
10 2 - print
```
yields `8`

- `*` : Multiply the two top stack elements
```pascal
3 4 * print
```
yields `12`

- `*` : Integer divide the two top stack elements
```pascal
10 3 / print
```
yields `3`

#### Stack editing
- `dup` : Duplicate the top stack element
```pascal
"Hello" dup print print
```
yields `Hello Hello`

- `drop` : Drop the top stack element
```pascal
"Hello" "World" drop print
```
yields `Hello`

- `2dup` : Duplicate the two top stack element
```pascal
10 2 2dup + print / print
```
yields `12 5`

- `2drop` : Drop the two top stack element
```pascal
"Hello" 10 "World" 2drop print
```
yields `Hello`

- `swap` : Swap the two top stack element
```pascal
"Bottom" "Top" swap print print
```
yields `Bottom Top`

- `over` : Copies the element second on the stack
```pascal
"Hello" "World" over print print print
```
yields `Hello World Hello`

#### Output
- `print` : Print the top stack element followed by newline
```pascal
"Hello" print "World" print
```
yields
```
Hello
World
```

- `put` : Print the top stack element without a newline
```pascal
"Hello" put "World" put
```
yields
```
HelloWorld
```

#### Logic
- `!` : Negates the top stack element
```pascal
False ! print
```
yields `True`

- `|` : Logical OR's the top two stack elements
```pascal
True False | print
```
yields `True`

- `&` : Logical AND's the top two stack elements
```pascal
True False & print
```
yields `False`

#### Comparison
- `=` : Tests equality on the top two stack elements
```pascal
True False = print
```
yields `False`

- `<` : Tests if the second stack element is smaller than the top stack element
```pascal
2 5 < print
```
yields `True`

- `<` : Tests if the second stack element is greater than the top stack element
```pascal
2 5 > print
```
yields `False`

- `<=` : Tests if the second stack element is smaller than or equal to the top stack element
```pascal
2 5 <= print
```
yields `True`

- `<=` : Tests if the second stack element is greater than or equal to the top stack element
```pascal
2 5 >= print
```
yields `False`

#### Comments
- `~` : Ignores the rest of the line
```pascal
123 123 + ~ Add the numbers together
```