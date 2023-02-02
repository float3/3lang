# lang
lang is a interpreted functional toy language written in F#

## why
this project was my first F# program as well as my first experience in making a language

## examples
the following are example programs written in lang

Fibonacci Numbers:
```fs
let fib = \b. 
    cond b < 2
    then b
    else fib (b-1) + fib (b-2)
in fib input
```

Factorial:
```fs
let a = (\b. 
    cond b < 2 
    then b 
    else b * a (b-1)) 
in a input
```

## installing and running a program

```bash
git clone https://github.com/float3/lang.git
cd ./lang
dotnet run "./examples/fib.3" <N> 
```

this will print the Nth fibonacci number using the example program

currently only one input (of type integer) is supported 
