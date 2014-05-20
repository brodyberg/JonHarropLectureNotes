let xs = [1..10]

let rec iter f xs =
   match xs with
   | [] -> ()
   | x::xs -> 
      f x
      iter f xs

iter (printfn "%d") xs

let rec map f xs = 
   match xs with
   | [] -> []
   | x::xs -> f x::map f xs

map (printfn "%d") xs |> ignore

// Seq.map does nothing up front

[ for x in [1..5] -> 
   x + 1 ]

// fold, ltr
// I have done so much homework, i should be really 
// proud of myself, so much wow!!!!
// F# syntax: matching, functions, seq, etc etc
// Functional: currying, map, iter, fold

let rec fold f a xs =
   match xs with
   | [] -> a
   | x::xs -> fold f (f a x) xs

fold (+) 0 [1..100]
fold (*) 1 [1..5]

// awesome!!!
fold max 0 [1..5]

List.fold (+) 0 [1..100]

// if you are working with something a million lines
// long use an array

// fine to use list: lots of short lists is ok, but one 
// big list will kill you

Seq.fold (+) 0 [1..100]

// where you want to use map vs. [ for ... do ] when you are doing 
// operation after operation after oepration
// so pipeline: 

x |> f
f x

// list.map is quicker than seq.map
// but nothing is done immediately
// but list.map creates intermediate datastructures that get thrown away

[1..10]
|> Seq.map (fun n -> 2*n)
|> Seq.map (fun n -> n*n)
|> Seq.fold (+) 0

[|1..10|]
|> Array.map (fun n -> 2*n)
|> Array.map (fun n -> n*n)
|> Array.fold (+) 0

// boom, parallel!
[|1..10|]
|> Array.Parallel.map (fun n -> 2*n)
|> Array.Parallel.map (fun n -> n*n)
|> Array.Parallel.fold (+) 0

// or combine stages
[|1..10|]
|> Array.Parallel.map (fun n -> 
    let n = 2*n
    n*n
|> Array.Parallel.fold (+) 0

// rule of thumb: do at least a thousand flops,
// then it will do really well
// otherwise will not do much vs. administration costs

// with pipe forward
// graph do: bfs, dfs, topological sorting

// forward pipeline is all about currying, since we require that
// for this to even work in the first place

// parallel vs. async

async { for n in 1..10 -> 
            let n = 2*n
            n*n }

[ for n in 1..10 -> 
   async { let n = 2*n
           return n*n } }
|> Async.Parallel
|> Async.RunSynchronously
|> Array.fold (+) 0
// multicores are very memory starved
// so the key is to optimize your memory accesses/locality
// drew memory heiarchcy
//
// each cpu each has queue of work
// aka task
// derived from CILK from MIT
// rather than one global work queue
// thread local queue partially thread-safe
// one end not thread safe
// when cpu is dry, pull off next
// if that task spawns more tasks
// when NEXT core runs dry he chooses another core
// work item at random and pulls from there
// so, super efficient, fine-grain load balancing
// all swings on the fact thtat when a task creates child tasks
// they are pushed onto the parent queue
// so it's more likely a child item runs on the queue the parented it
// but it can still run on aother core

// async is totally different
// it thread hops
// waits
// no affinity between core launching vs. catching
// so, highly unlikely to be using caching efficiently
// so use tpl to run locally fast FAST
// if you want to write parallel code you don't want to use async {} 
// only use async if you are making call that's orders of magnitude slower (Azure)
// parallel is about throughput, not latency
// async is about concurrency, lots of independent behaviors appearing to
// operate async, runs hundreds of times slower in terms of throughput
// holy shit, async is mega slow compared to parallel

// map is about 10x slower than .net dictionary
// so use .net dictionary!!! better than C++

// pro-tip: there's no immutable hashtable

// why f# map? f# compiler - pass it to other people that 
// can't mutate it

// ActivePatterns, even better matching!
// provide a view of a value, but without creating a datastructure for it
let (|Less|Equal|Greater|) c =
    if c < 0 then Less
    elif c=0 then Equal
    else Greater

match compare 3 4 with
| Less -> "less"
| Equal -> "equal"
| Greater -> "greater"

type Comparison = Less | Equal | Greater

// generate representation in the matcher
// a view of the integers

// Choice<unit, unit, unit> is internal representation
// unit represents the arguments passed into the Less, Equal or Greater
// clauses in compare

// but there's no type, so the
// but once you define the thing in the active pattern it's real and 
// you can write whatever you want against it

// can't create a value of the pattern, can only match on it like 
// it really exists
// you CAN create values from discriminated unions

// complete match pattern
// if you want an escape: 

let alpha = set ['a'..'z'] + set ['A'..'Z']
let digit = set ['0'..'9']
let alphanum = alpha + digit

let (|Char|_|) alphabet chars =
    match chars with
    | [] -> None
    | x::xs when Set.contains x alphabet -> Some x
    | _ -> None

let (|Char|_|) alphabet chars =
    match chars with
    | [] -> None
    | x::xs when Set.contains x alphabet -> Some(x,xs)
    | _ -> None

// multiple versions of things here
match ['H'; 'e'; 'l'; 'l'; 'o';] with 
| Char alpha c -> Some c
| _ -> None

// fails because 3 is not in the alphabet
match ['3'; 'e'; 'l'; 'l'; 'o';] with 
| Char alpha c -> Some c
| _ -> None

match ['H'; 'e'; 'l'; 'l'; 'o';] with 
// call our partial active pattern
| Char alpha (c0, Char alpha (c1, cs)) -> Some(c0, c1, cs)
| _ -> None

match ['H'; 'e'; 'l'; 'l'; 'o';] with 
// call our partial active pattern
| Char alpha (c0, Char alpha (c1, Char alpha c2)) -> Some(c0, c1, cs)
| _ -> None

//////

let rec (|Chars|) alphabet chars =
    match chars with
    | Char alphabet (c, Chars alphabet (cs, xs)) -> 
        (c::cs, xs)
    | xs -> [], xs

match ['H'; 'e'; 'l'; 'l'; 'o'] with 
| Chars alpha (cs, xs) -> cs, xs


List.ofSeq "Hello world!"
// ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!']

match ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with 
| Chars alpha (cs, xs) -> cs, xs


// used to create DSL - whole lexing and parsing is about 100 lines of code
// automates an enormous amount of work for actuaries to go into production


let (|Ident|_|) chars = 
    match chars with
    | Char alpha (c, Chars alphanum (cs, xs)) -> 
        Some(c::cs, xs)
    | _ -> None

match ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with 
| Ident (cs, xs) -> Some(cs, xs)
| -> None

match ['H'; 'e'; 'l'; 'l'; 'o'; '1'; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with 
| Ident (cs, xs) -> Some(cs, xs)
| -> None

match ['3'; 'e'; 'l'; 'l'; 'o'; '1'; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with 
| Ident (cs, xs) -> Some(cs, xs)
| _ -> None

// can have lexical tokens be an active pattern only, funky but awesome

// use maps for nested scopes and backtracking
// map at each level of scope
// functional maps can co-exist, so you revert to using an old one
// and go back to it

//////////////////////////////////////////
//
// OO
//

// whole dsl was purely functional 
// just dealt with numbers, no collections, or controls or whatever
// for errors they had business requirements for error conditions like 
// illegally large dollar amounts
// parser can parse whatever you send in, 
// interpreter can handle whatever was parsed

// OO for real

type Vec2(x:float, y:float) =
    member this.X = x
    member this.Y = y

(Vec2(3.0, 4.0))
(Vec2(3.0, 4.0)).X

type Vec2(x:float, y:float) =
    member private this.X = x
    member this.Y = y

(Vec2(3.0, 4.0))

type Vec2(x:float, y:float) =
    member this.X = x
    member this.Y = y
    member this.Length = sqrt(x*x + y*y)

(Vec2(3.0, 4.0)).Length

// object expressions

{ new System.Object() with
    member x.ToString() = "F#!" }

// awesome
do
    use myDisposable =
        { new System.IDisposable with
            member this.Dispose() = printfn "Disposed" }
    ()

let myByte =
    let path = ""
    use stream = System.IO.File.OpenRead path
    stream.ReadByte()

seq { use stream = System.IO.File.OpenRead ""
      while true do
        yield stream.ReadByte() }

let readLines file =
    seq { use stream = System.IO.File.OpenRead ""
          use reader = System.IO.StreamReader(stream)
          while not reader.EndOfStream do
            yield stream.ReadByte() }

// implemented above, which was typical code pre-.NET 4.0
System.IO.File.ReadLines

System.IO.Directory.GetFiles(".")

///////////////////////////////////////////////////////


type Vec2(x:float, y:float) =
    member this.X = x
    member this.Y = y
    member this.Length = sqrt(x*x + y*y)

type Vec2 = 
    { x: float
      y:float}  
    member this.Length = sqrt(x*x + y*y)

{ x = 3.0; y = 4.0; }.Length

type State = 
    | On 
    | Off
    member this.ToInt() =
        match this with
        | On -> 3
        | Off -> 7

On.ToInt()

type Vec3 = 
    { mutable x: float
      mutable y: float }
    
    member this.Length 
        with get() = sqrt(this.x*this.x + this.y*this.y)
        and set length =
            let scale = length / this.Length
            this.x <- scale * this.x
            this.y <- scale * this.y

let u = { x = 3.0; y = 4.0 }


u.Length

u.Length <- 1.0

u

type Vec3 = 
    { mutable x: float
      mutable y: float }
    
    member this.Length 
        with get() = sqrt(this.x*this.x + this.y*this.y)
        and set length =
            let scale = length / this.Length
            this.x <- scale * this.x
            this.y <- scale * this.y

    member this.Item
        with get i =
            if i=0 then this.x else this.y
        and set i v =
            if i=0 then
                this.x <- v
            else
                this.y <- v

let u = { x = 3.0; y = 4.0 }

u.[0]
u.[1]

u.[1] <- -4.0

u

////////

type Vec3 = 
    { mutable x: float
      mutable y: float }
    
    member this.Length 
        with get() = sqrt(this.x*this.x + this.y*this.y)
        and set length =
            let scale = length / this.Length
            this.x <- scale * this.x
            this.y <- scale * this.y

    member this.Item
        with get i =
            if i=0 then this.x else this.y
        and set i v =
            if i=0 then
                this.x <- v
            else
                this.y <- v

    static member (+) (u, v) = { x = u.x + v.x; y = u.y + v.y }
    static member (*) (s, v) = { x = s * u.x; y = s * u.y }

let u = { x = 3.0; y = 4.0 }
let v = { x = 5.0; y = 9.0 }

u + v

2.0 * u

// embedded DSL
// overload plus and multiply etc operators

type Expr =
    | Int of int
    | Var of string
    | Add of Expr * Expr
    | Mul of Expr * Expr

    static member (+) (f, g) = Add(f, g)
    static member (*) (f, g) = Mul(f, g)
    static member (-) (f, g) = Mul(f, Mul(Num -1.0, g))

    // he then goes on to completely rewrite the nestish stuff
    // with symbols

d (f, "x") // complicated

type Expr =
    | Int of int
    | Var of string
    | Add of Expr * Expr
    | Mul of Expr * Expr

    static member (+) (f, g) =
        match f, g with
        | Num 0.0, f // OR PATTERN
        | f, Num 0.0 -> f
        | f, g -> Add(f, g)
        
    static member (*) (f, g) =
        match f, g with
        | Num 0.0, _
        | _, Num 0.0 -> Num 0.0 // OR PATTERN
        | Num 1.0, f
        | f, Num 1.0 -> f
        | f, g -> Mul(f, g)

    static member (-) (f, g) = Mul(f, Mul(Num -1.0, g))

d (f, "x") // way simpler

// and in the nest combinator

let rec nest n f x =
    if n=0 then x else
        nest (n-1) f (f x)

////////////////

// useful for images
Array.init 10 (fun n -> n)
Array2D.init 10 10 (fun i j -> i + j)

Array2D.init 10 10 (fun i j -> i + j)
|> Array2D.iteri (fun i j x -> printfn "%A %A %A" i j x)

////////////////
// SAME!!!!
new System.Uri("http://www.google.com")
System.Uri("http://www.google.com")
// crazy!!!
// but keep for IDisposable, keep since you're getting rid of it
// otherwise nothing special about it, shouldn't need to know
// that it's even an object

let readLines file =
    seq { use stream = System.IO.File.OpenRead ""
          use reader = System.IO.StreamReader(stream)
          while not reader.EndOfStream do
            yield stream.ReadByte() }

// pattern matching is optimized by the compiler so even if you
// did implement something in C# it wouldn't be optimized

Seq.iter (printfn "%A") [1..5]
Seq.iter (printfn "%A") [|1..5|]
// you can't do this in OCAML, this is from .NET 

// Jon's assessment: graph libraries are really bad
// none in any language!
// each language has an angle
// none are perfect
// .net can't express abstract algorithms over graph type
// functor takes class and returns class
// F# it tends to be that you get a solid collection of algorithms
// but there's only way to represent your graph (dictionary with hashsets)
// but he wants flexibility

// csharp has its own way of using events
// fsharp just uses functions
// find bits of code in your codebase where this will pay big dividends
// with little changes
// his graph algorithms look imperative
// so not a big payoff
// big payoff: union types
// mistake: start from scratch and write everything in fsharp
