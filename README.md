# bowling-haskell

My first (arguably) non-trivial Haskell.  It scores a bowling game. This is probably stylistically terrible but over 
time as I learn more idiomatic Haskell I improve it.  It can also be utilized via
an untested, poorly functioning larval webserver also written in Haskell.

### How To Run

1. Install Stack
1. Run ```stack build --ghc-options="-Wall -Werror"``` inside the project directory
1. Run ```stack bowling-haskell-exe``` inside the project directory
1. Input numbers between 0 and 10 separated by spaces
1. Press ```Enter```

\* I'm not certain that I haven't missed something

### How To Test
1. Install Stack
1. Run ```stack test```
