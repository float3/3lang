# 3lang
3lang is a functional toy language

## why
this project was a practice in making a language and in writing F#

## quirks
in 3lang the only numeric literal allowed is '3' 
so you have to construct any number you need from 3

## examples
the following is a example program written in 3lang

```fs
let fib = \b. 
    cond b < (3/3+3/3)
    then b
    else fib (b-3/3) + fib (b-3/3-3/3)
in
fib (3*3)
```

returns the 9th (3*3) fibonacci number
