# Gradually-Typed Lambda Calculus

This program implemented an interpreter for the gradually-typed lambda calculus 
using the eager coercion calculus and the UD blame assignment. 
To run this program, make sure that Haskell and the Stack build tool are 
installed on the system. 

### 1. How to install Stack
    
   For Unix operating systems, run one of these commands:  

        curl -sSL https://get.haskellstack.org/ | sh  
         
   or   
   
        wget -qO- https://get.haskellstack.org/ | sh  

   If [homebrew](https://brew.sh/) is available, run:
   
        brew install haskell-stack  
        
   For Windows, download and install [Windows 64-bit Installer](https://www.stackage.org/stack/windows-x86_64-installer)  

### 2. How to build and run the program

   Go to the program directory, then run the following command to build the program:  
   
        stack build  

   To start the GTLC interpreter, run the command:

        stack exec GTLC-exe  

### 3. The Language Syntax
    
   #### Types

   Base types:
   
        Nat
        Bool

   Dynamic type:

        Dyn

   Reference types:

        Ref T 
       
   For function types, use "->" between any two types. For example:
   
        Nat->Bool

   #### Expressions

   Constants include boolean values and 0:
        
        true
        false
        0

   References are denoted with the keyword "ref", which accepts any expression e as the argument:

        ref e

   Dereferences accept any expression as the argument. Use the symbol "!" to dereference an expression e:

        !e

   Assignments accept any expression on both sides of the symbol ":=". 

        e1 := e2

   For lambda-abstraction λx:T. t, use "\" to denote λ. To specify the type of the bound variable, 
   insert a colon and a type after the variable name. The body expression is placed after a dot. For example, λx:Nat. x is written as:   
   
        \x:Nat. x

   You can also omit type annotations to denote dynamically-typed variables. The colon and a type 
   are no longer needed after the variable name. 

        \x. x

   Another option to denote a dynamically-typed variable is to annotate it as dynamic.

        \x:Dyn. x
        
   For a function application, insert a space between the operator and the argument. For example, 
   (λx:Nat->Nat. x) (λx:Nat. x)is written as:   
   
        (\x:Nat->Nat. x) (\x:Nat. x) 

   Arithemtic operators (succ, pred, iszero) accept any expression e as the argument:    
   
        succ e
        pred e
        iszero e
        
   Conditionals:
   
        if e1 then e2 else e3
        
### 4. How to exit the program

        Type "exit" at the command prompt.

### 5. How to run tests on the program

        Type "test" at the command prompt.

   Note: There are 35 test cases. Each test case follows the following format:

        Test> Input expression 
        ==> Modified expression with additional coercions 
        Result

   Results can be of three kinds: 
   
   1. a value, printed in green
   2. a static type error, printed in red ("Type error")
   3. a runtime cast exception, printed in red ("Invalid cast exception")
