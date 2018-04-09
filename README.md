# Gradually Typed Lambda Calculus

This program implemented an interpreter for the simply typed lambda calculus using 
the formal definitions and the OCaml implementation described in the textbook, 
"Types and Programming Languages" by Benjamin C. Pierce. To run this program, make 
sure that Haskell and the Stack build tool are installed on the system. 

**1. How to install Stack** 
    
   For Unix operating systems, run one of these commands:  

        curl -sSL https://get.haskellstack.org/ | sh  
         
   or   
   
        wget -qO- https://get.haskellstack.org/ | sh  

   If [homebrew](https://brew.sh/) is available, run:
   
        brew install haskell-stack  
        
   For Windows, download and install [Windows 64-bit Installer](https://www.stackage.org/stack/windows-x86_64-installer)  

**2. How to build and run the program**

   Go to the program directory, then run the following commands in the same order:  
   
        stack build  
        stack exec GTLC-exe  


**3. How to use the program**

   For constants like true, false, and 0, type:
        
        true
        false
        0
        
   For lambda-abstraction λx:T. t, use "\" to denote λ. For example, λx:Nat. x is written as:   
   
        \x:Nat. x 
        
   For application t t, insert a space between the two terms. For example, (λx:Nat->Nat. x) (λx:Nat. x) is written as:   
   
        (\x:Nat->Nat. x) (\x:Nat. x) 

   For arithemtic expressions, type:    
   
        succ 0
        pred 0
        iszero 0
   
   For records such as {a=true,b=succ (succ 0)}, type:
   
        {a=true,b=succ (succ 0)}
        
   For projections, use the "." operator along with a label name, as follows:
   
        {a=true,b=succ (succ 0)}.a
        
   For conditionals, type:
   
        if true then 0 else pred 0
   
   For base types such as Nat, Bool, and Top, type:
   
        Nat
        Bool
        Top 
       
   For function types, use "->" between the two types, as follows:
   
        Bool->Bool
        
   For record types such as {a:Bool,b:Nat}, type:
   
        {a:Bool,b:Nat}
        
**4. How to exit the program**

        Type "exit" in the console

**5. How to run tests on the program**

   Run the command:  
   
        stack test
