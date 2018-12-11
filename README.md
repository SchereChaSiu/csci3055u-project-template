# _Haskell_

- Gajan Sivanesan
- Gajan.Sivanesan@uoit.net

## About the language

Haskell is a standardized general-purpose, purely functional programming language with non-strict semantics and strong static typing. It gets its name from Haskell Curry. There are multiple iterations of Haskell  

# History

## Haskell 1.0 to 1.4 (1990 – 98)
------------------------------

   The first version of Haskell was defined in 1990. It had multiple series of language definitions (1.0 – 1.4). 
		
## Haskell 98 (1998 – 2010)
------------------------

   In late 1997, Haskell 98 was created. This was intended to be a stable, minimal, and portable version of the language. It also contains a library for teaching. This version would be the base for the next iterations of the language. The committee also welcomed creating extensions and variants of Haskell 98 by adding and incorporating experimental features. 
		
## Haskell 2010 (2010 – )
----------------------

   In early 2006, the committee started defining a successor to Haskell 98. This was going to be an incremental process meaning the language would get revised and updated once a year. In November 2009, Haskell 2010 was announced and released in July 2010. This version contained changes and additions like:
		
1.	Hierarchical Module Names 
2.	The Foreign Function Interface 
3.	Update to the rules of type interference 
4.	Changed grammar error in syntax

# Features

* Haskell features lazy evaluation, lambda expressions, pattern matching, list comprehension, type classes and type polymorphism.

* It is a pure functional programming language

* Has a strong static type system

* Has an active and growing community with more than 5,4000 third-party open-source libraries and tools. 



## About the syntax

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

Haskell uses the Glasgow Haskell Compiler (GHC). It is an open-source native code compiler for Haskell. The lead developers are Simon Peyton Jones and Simon Marlow. It supports many extensions and libraries as well as having optimizations that streamline the process of generating and executing code. GHC is written in Haskell however the runtime system is written in C and C-. It supports both Haskell 98 and Haskell 2010. 

## About the standard library

> _Give some examples of the functions and data structures
> offered by the standard library_.

Class ```Array``` constructs an immutable array from a pair of bounds and a list of initial associations.

``` haskell
import Data.Array

myArray = array (1, 3) [(1, "a"), (2, "b"), (3, "c")] -- create an array

main = do
    print myArray -- prints the array
    -- > array (1,3) [(1,"a"),(2,"b"),(3,"c")]
    print $ myArray ! 2 -- value at the given index
    -- > "b"
    print $ bounds myArray -- bounds with which the array is constructed
    -- > (1,3)
    print $ indices myArray -- list of indecies in ascending order
    -- > [1,2,3]
    print $ elems myArray -- list of elements in index order
    -- > ["a","b","c"]
    print $ assocs myArray -- list of associations of an array in index order
    -- > [(1,"a"),(2,"b"),(3,"c")]
 ```
 
Class ```Enum``` defines operations on sequentially ordered types.

```haskell
main = do
    print $ succ 'a' -- successor of value
    -- > 'b'
    print $ pred 'b' -- predecessor of value
    -- > 'a'
    print $ (toEnum 65 :: Char) -- convert from an int to a char
    -- > 'A'
    print $ fromEnum 'B' -- convert to an int
    -- > 66

    print $ take 10 $ enumFrom 'a' -- take the first 10 char starting from 'a'
    print $ take 10 $ ['a'..] -- take the first 10 char starting from 'a'
    -- > "abcdefghij"

    print $ take 10 $ enumFromThen 'a' 'c' -- take the first 10 characters with the pattern with 'a' and 'c' (so every other character)
    print $ take 10 $ ['a', 'c'..] -- take the first 10 characters with the pattern with 'a' and 'c' (so every other character)
    -- > "acegikmoqs"

    print $ enumFromTo 'a' 'e' -- sequence of values with the ranges 
    print $ ['a'..'e'] -- sequence of values with the ranges
    -- > "abcde" 

    print $ enumFromThenTo 'a' 'c' 's' -- sequence of every other value stopping at 's'
    print $ ['a', 'c'..'s'] -- sequence of every other value stopping at 's'
    -- > "acegikmoqs"
```

Class ```Map``` creates and manipulates maps

``` haskell 
import qualified Data.Map as Map

phoneBook = Map.fromList [(1992, "Gajan"), (2018, "Sivanesan")] -- create a map

main = do
    print phoneBook -- print map
    -- >  [(1992,"Gajan"),(2018,"Sivanesan")]
    print $ Map.lookup 1992 phoneBook -- output element with matching key
    -- > "Gajan"
    print $ (Map.empty :: Map.Map Int Int) -- empty map
    -- > []
    print $ Map.singleton 1 2 -- >  a mapt with a single element 
    -- > [(1,2)]
    print $ Map.insert 4 "abc" Map.empty -- > insert in map, if arleady there replace the element
    -- > [(4,"abc")]
    print $ Map.null phoneBook -- checks if map is empty 
    -- > False
    print $ Map.size phoneBook -- size of the map 
    -- > 2
    print $ Map.toList phoneBook -- convert to a list of key/value pairs
    -- > [(1992,"Gajan"),(2018,"Sivanesan")]
    print $ Map.keys phoneBook -- find the keys of the map 
    -- > [1992,2018]
    print $ Map.elems phoneBook -- find the elements of the map 
    -- > ["Gajan","Sivanesan"]
```

Class ```Tree``` creates and manipulates trees

``` haskell
import Data.Tree

tree = Node "A" [Node "B" [], Node "C" [Node "D" [], Node "E" []]] -- create tree

main = do
    print tree -- print tree
    putStrLn $ drawTree tree -- draws the tree 
    {- 
A
|
+- B
|
`- C
   |
   +- D
   |
   `- E
-}
    putStrLn $ drawForest $ subForest tree -- draws the forests and sub forests
    {- 
B

C
|
+- D
|
`- E 
-}
    print $ flatten tree -- flattens the tree 
    -- > ["A","B","C","D","E"]
    print $ levels tree -- level of tree
    -- >[["A"],["B","C"],["D","E"]]
```

## About open source library

> _Describe at least one contribution by the open source
community written in the language._

**Hackage** is the Haskell community’s central package archive of open source software. http://hackage.haskell.org/

**gtk:** is the core library of the Gtk2Hs suite of libraries for Haskell based on Gtk+. Gtk+ is an extensive and mature multi-platform toolkit for creating graphical user interfaces. http://hackage.haskell.org/package/gtk

*simple GUI implementation that ouputs "HelloWorld"*

``` haskell 
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.GI.Base
import qualified GI.Gtk as Gtk

main :: IO ()
main = do 
    Gtk.init Nothing

    win <- new Gtk.Window [#title :="HelloWorld"]
    on win #destroy Gtk.mainQuit
    #showAll win

    Gtk.main

``` 
*more packages from **Hackage***

**http-client:** An HTTP client engine, intended as a base layer for more user-friendly packages. http://hackage.haskell.org/package/http-client

**yaml:** Support for parsing and rendering YAML documents. https://github.com/snoyberg/yaml#readme

**aeson:** A JSON parsing and encoding library optimized for ease of use and high performance. http://hackage.haskell.org/package/aeson

# Analysis of the language

> _Organize your report according to the project description
document_.

1. Haskell is a pure functional language. It supports parametric polymorphism (ala C++ templates, but more powerful), and 
ad-hoc polymorphism through type classes.

2. Template Haskell is a GHC extension to Haskell that adds compile-line meta-programming facilities. It has been included in
GHC since version 6. Template Haskell allows the user to convert back and forth between concrete syntax and abstract syntax 
trees.

3. Haskell creates functions with free variables everywhere, thus, closures are created all the time. Some closures are
optimized out by the compiler.

```haskell f x = (\y -> x + y) -- f returns a closure```

4. Haskell supports lexical scoping but with the use of an extension to the type class system of Haskell, dynamic scoping can be
used. This is done by adding a “constraint” to the type making it dynamic. Due to the possible issues that dynamic scoping has,
it is strongly encouraged NOT to this type of scoping. These constraints lead to types of the form ``` haskell (?x::t') => t ```
, which says "this function uses a dynamically-bound variable ?x of type t' "

```haskell sort :: (?cmp :: a -> a -> Bool) => [a] -> [a] -- dynamic scoping ```

5. Haskell is a fully functional based program. The constructs are in the language itself. As for all functional languages the
constructs in the language are simple and easy implement.

6. Haskell has strong typing. Haskell’s typing is also static. This mean that the type of every expression and every variable is 
knows at compile time. However, Haskell’s interpreters and compilers are smart enough to infer the types of your expressions 
most of the time, so defining variables doesn’t have to happen all the time. 

7. 
	**Strengths:** 
		Good for people that think mathematically 
		Excellent type safety
		Executables have high performance 
		Interactive environment (hugs/ghci)
		Parallelism (using ```haskell par``` and ```haskell pseq```
		Supportive community 
	
	**Cons:** 
		Hard to learn 
		Libraries not documented well 
