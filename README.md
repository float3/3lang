# 3lang
3lang is a interpreted functional toy language written in F#

## why
this project was my first F# program as well as my first experience in making a language

## quirks
in 3lang the only numeric literal allowed is '3' 
so you have to construct any number you need from 3

## examples
the following are example programs written in 3lang

Fibonacci Numbers:
```
let fib = \b. 
    cond b < (3/3+3/3)
    then b
    else fib (b-3/3) + fib (b-3/3-3/3)
in fib input
```

Factorial:
```
let a = (\b. 
    cond b < (3/3+3/3) 
    then b 
    else b * a (b-(3/3))) 
in a input
```

## installing and running a program

```cmd
git clone https://github.com/Float3/3lang.git
cd ./3lang
dotnet run "./examples/fib.3" <N> 
```

this will print the Nth fibonacci number using the example program

currently only one input (of type integer) is supported 