# _Haskell_

- Gajan Sivanesan
- Gajan.Sivanesan@uoit.net

## About the language

Haskell is a standardized general-purpose, purely functional programming language with non-strict semantics and strong static typing. It gets its name from Haskell Curry. There are multiple iterations of Haskell  
	 Haskell 1.0 to 1.4 (1990 – 98)
		The first version of Haskell was defined in 1990. It had multiple series of language definitions (1.0 – 1.4). 
	Haskell 98 (1998 – 2010)
		In late 1997, Haskell 98 was created. This was intended to be a stable, minimal, and portable version of the language. It also contains a library for teaching. This version would be the base for the next iterations of the language. The committee also welcomed creating extensions and variants of Haskell 98 by adding and incorporating experimental features. 
	Haskell 2010 (2010 – )
		In early 2006, the committee started defining a successor to Haskell 98. This was going to be an incremental process meaning the language would get revised and updated once a year. In November 2009, Haskell 2010 was announced and released in July 2010. This version contained changes and additions like:
1.	Hierarchical Module Names 
2.	The Foreign Function Interface 
3.	Update to the rules of type interference 
4.	Changed grammar error in syntax


## About the syntax

> _give some code snippet of the language_

*How to print "Hello, World!"*

```haskell
main = putStrLn "Hello, World!"
```
*Sum a number with itself*

```haskell 
doubleItself x = x + x 

main = putStrLn (show (doubleItself 6))
-- >12
```
*Basic if-else statement*

```haskell
doubleSmallNumber x = if x > 100
                    then x
                    else x*2

main = putStrLn (show(doubleSmallNumber 5))
-- >10
```

## About the tools

> _Describe the compiler or interpreter needed_.

## About the standard library

> _Give some examples of the functions and data structures
> offered by the standard library_.

## About open source library

> _Describe at least one contribution by the open source
community written in the language._

# Analysis of the language

> _Organize your report according to the project description
document_.


