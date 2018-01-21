# Improved Couscous 
*BNF Grammar parser and disambiguator written with Haskell's parser combinators*

- - -

It will eventually compile the given grammar to a parser for the language described
by the grammar itself.

## Syntax 
As many authors tend to use slightly different versions of the same notation,
I adopted the following syntax for the BNF grammar:

+ A terminal symbol is any literal enclosed in double quotes
    - ``` "+", "(", "literal" ```
+ A non-terminal symbol is an identifier enclosed in angular brackets: 
    -   ``` <int>, <expr>, <lift2> ```
+ An identifier is any string defined as in most formal languages, namely 
an alphabetic character followed by zero or more alphanumeric characters
+ An expression is a concatenation of symbols (terminal or non-terminal) 
    - ``` <int>"+"<int> ```
+ A production is described by a non-terminal symbol followed by the string 
``` ::= ``` and an arbitrary sequence of expressions alternated 
by the character ``` | ```


## Left Recursion
Currently the parser provides the ability to eliminate (direct) left recursion, 
the following is an example of that behaviour:

### Before
```
<S> ::= <expr> 
<expr> ::= <expr> "+" <term>  | <expr> "-" <term>  | <term> 
<term> ::= <term> "*" <factor>  | <term> "/" <factor>  | <factor> 
<factor> ::= "(" <expr> ")"  | <int> 
```

### After
```
<S> ::= <expr> 
<expr'> ::= "+" <term> <expr'> 
<expr'> ::= "-" <term> <expr'> 
<expr> ::= <term> <expr'> 
<term'> ::= "*" <factor> <term'> 
<term'> ::= "/" <factor> <term'> 
<term> ::= <factor> <term'> 
<factor> ::= "(" <expr> ")"  | <int> 
```

