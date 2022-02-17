open System

type Token = 
    | Plus | Minus | Slash | Star | Equals
    | BackSlash | Dot
    | Lparen | Rparen 
    | Integer of int //| String of string | Char of char
    | Identifier of string
    | Lett | In

type Tree =
    | Operator of Tree * Token * Tree
    | Literal of int
    | Var of string
    | Let of string * Tree * Tree
    | Lamb of string * Tree
    | App of Tree * Tree

type Value =
    | Λ of string * Tree * Map<string,Value>
    | Int of int

let isLetter a =
    a >= 'a' && a <= 'z'

let isThree a =
    a = '3'

let isNumber a =
    a >= '0' && a <= '9'

let isValidNumber a =
    if isNumber a && not(isThree a) 
    then failwith "the only number literal you're allowed to use is 3"
    else isThree a 

let rec eatWhile f s =
    match s with
    | h::t -> 
        if f h 
        then
            let a,b = eatWhile f t
            string(h) + a,b 
        else "",s
    | [] -> "",[]

let rec lex s =
    //printfn "lex%A" s
    match s with
    | h::t -> 
        match h with
        | '+' -> Plus::lex t | '-' -> Minus::lex t  | '/' -> Slash::lex t  | '*' -> Star::lex t  | '=' -> Equals::lex t 
        | '(' -> Lparen::lex t | ')' -> Rparen::lex t 
        | '.' -> Dot::lex t | '\\' -> BackSlash::lex t
        | w when isLetter w ->
            let a,b = eatWhile isLetter s
            match a with
            | "let" -> Lett::lex b
            | "in" -> In::lex b
            | _ -> Identifier(a)::lex b
        | w when isValidNumber w ->
            let a,b = eatWhile isValidNumber s
            (a |> int |> Integer)::lex b
        | _ -> lex t
    | [] -> []

let rec parseBot toks =
    //printfn "Bot%A" toks
    match toks with
    | (Lparen)::t ->
        let a,b = parseExpr t
        a,List.tail b
    | (Identifier h)::t -> Var(h),t
    | (Integer h)::t -> Literal(h),t
    | _ ->  printfn "parseBot%A" toks
            failwith "write better code"

and parseApp toks =
    //printfn "App%A" toks
    let a,b = parseBot toks
    match b with
    | (Lparen)::t
    | (Identifier _)::t 
    | (Integer _)::t ->
        let c,d = parseApp b
        App(a,c),d 
    | _ -> a,b

and parseTerm toks =
    //printfn "Term%A" toks
    match toks with
    | Lett::Identifier(a)::Equals::t -> 
        let tree,toks = parseExpr t
        let rest,var = parseExpr (List.tail toks)
        Let(a,tree,rest),var
    | BackSlash::Identifier(a)::Dot::t ->
        let tree,toks = parseExpr t
        Lamb(a,tree),toks
    | _ -> parseApp toks

and parseFactor toks =
    //printfn "Factor%A" toks
    let ltree,toks = parseTerm toks
    match toks with
    | Star::t -> 
        let rtree,toks = parseFactor t
        Operator(ltree,Star,rtree),toks
    | Slash::t -> 
        let rtree,toks = parseFactor t
        Operator(ltree,Slash,rtree),toks
    | _ -> ltree,toks

and parseExpr toks =
    //printfn "Expr%A" toks
    let ltree,toks = parseFactor toks
    match toks with
    | Plus::t ->
        let rtree,toks = parseExpr t
        Operator(ltree,Plus,rtree),toks
    | Minus::t -> 
        let rtree,toks = parseExpr t
        Operator(ltree,Minus,rtree),toks
    | _ -> ltree,toks


let rec eval env tree =
    match tree with
    | Literal(v) -> Int(v)
    | Operator(l,tok,r) ->
        match (eval env l),(tok),(eval env r) with 
        | Int(l),Plus,Int(r) -> Int(l + r)
        | Int(l),Minus,Int(r) -> Int(l - r)
        | Int(l),Star,Int(r) -> Int(l * r)
        | Int(l),Slash,Int(r) -> Int(l / r)
        | Int(l),Equals,Int(r) -> if l = r then Int(1) else Int(0)
        | _ -> failwith "(:"
    | Let(i,tr,tre) -> 
        let env = Map.add i (eval env tr) env
        eval env tre
    | Var(i) -> 
        printf"%A" i
        Map.find i env
    | Lamb(i,tre) -> Λ(i,tre,env)
    | App(tr,tre) -> 
        match eval env tr with
        | Λ(i,tree,env) -> 
            let input = eval env tre
            let env = Map.add i input env
            eval env tree
        | _ -> failwith "unlucky"

let rec buildString input =
    match input with
    | h::t -> h + buildString t
    | _ -> ""

[<EntryPoint>]
let main args = 
    let input = if args.Length = 0 
                then Seq.toList (System.IO.File.ReadLines("./examples/fib.3"))
                else Seq.toList (System.IO.File.ReadLines(args.[0]))
    for line in input 
        do line
        |> Seq.toList
        |> lex 
        |> parseExpr 
        |> (fst>>eval Map.empty) 
        |> printfn "%A"
    0