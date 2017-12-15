# Simply Typed Lambda Calculus

This program implemented an interpreter for the simply typed lambda calculus using 
the formal definitions and the OCaml implementation described in the textbook, 
"Types and Programming Languages" by Benjamin C. Pierce. To run this program, make 
sure that Haskell and the Stack build tool are installed on the system. 

**1. How to install Stack** 
    
   For Unix operating systems, run one of these commands:  

         curl -sSL https://get.haskellstack.org/ | sh  
         
   or   
   
        `wget -qO- https://get.haskellstack.org/ | sh`  

   If [homebrew](https://brew.sh/) is available, run:
   
        brew install haskell-stack  
        
   For Windows, download and install [Windows 64-bit Installer](https://www.stackage.org/stack/windows-x86_64-installer)  

**2. How to build and run the program**

   Go to the program directory, then run the following commands in the same order:  
   
        stack build  
        stack exec STLC-exe  

**3. How to exit the program**

    Type "exit" in the console

**4. How to run tests on the program**

   Run the command:  
   
           stack test
