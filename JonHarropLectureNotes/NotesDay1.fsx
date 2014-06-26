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

// With eta-reduction: 
map (printfn "%d") xs |> ignore
// Without eta-reduction:
map (fun x -> printfn "%d" x) xs |> ignore

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

// With eta-reduction
fold (+) 0 [1..100]
fold (*) 1 [1..5]

// Without eta-reduction
fold (fun a b -> a + b) 0 [1..100]
fold (fun a b -> a * b) 0 [1..100]

// awesome!!!
fold max 0 [1..5]

List.fold (+) 0 [1..100]

// if you are working with something a million lines
// long use an array

// fine to use list: lots of short lists is ok, but one 
// big list will kill you

Seq.fold (+) 0 [1..100]

// where you want to use map vs. [ for ... do ] when you are doing 
// operation after operation after operation
// so pipeline: 
// 
// x |> f
// f x

// list.map is quicker than seq.map
// but nothing is done immediately
// but list.map creates intermediate datastructures that get thrown away

[1..10]
|> Seq.map (fun n -> 2*n)
|> Seq.map (fun n -> n*n)
|> Seq.fold (+) 0

// Now with eta-reduction:
let n2 n = 2 * n
let nn n = n * n

[1..10]
|> Seq.map n2
|> Seq.map nn
|> Seq.fold (+) 0

[|1..10|]
|> Array.map (fun n -> 2*n)
|> Array.map (fun n -> n*n)
|> Array.fold (+) 0

// boom, parallel!
[|1..10|]
|> Array.Parallel.map (fun n -> 2*n)
|> Array.Parallel.map (fun n -> n*n)
|> Array.fold (+) 0

// Any two maps can be composed

// or combine stages
[|1..10|]
|> Array.Parallel.map (fun n -> 
    let n = 2*n
    n*n)
|> Array.fold (+) 0

// rule of thumb: do at least a thousand flops,
// then it will do really well
// otherwise will not do much vs. administration costs

// with pipe forward
// graph do: bfs, dfs, topological sorting

// forward pipeline is all about currying, since we require currying
// for the forward pipeline to even work in the first place

// parallel vs. async

[ for n in 1..10 -> 
   async { let n = 2 * n
           return n * n } ]
|> Async.Parallel
|> Async.RunSynchronously
|> Array.fold (+) 0

// multicores are very memory starved
// so the key is to optimize your memory accesses/locality
// drew memory heiarchcy
//
// each cpu each has queue of work
// aka task
// derived from CILK from MIT (http://supertech.csail.mit.edu/cilk/)
// rather than one global work queue
// thread local queue partially thread-safe
// one end not thread safe
// when cpu is dry, pull off next
// if that task spawns more tasks
// when NEXT core runs dry it chooses another core
// work item at random and pulls from there
// so, super efficient, fine-grain load balancing
// all swings on the fact thtat when a task creates child tasks
// they are pushed onto the parent queue
// so it's more likely a child item runs on the queue the parented it
// but it can still run on another core

// async is totally different
// it thread hops
// waits
// no affinity between core launching vs. catching
// so, highly unlikely to be using caching efficiently
// so use TPL to run locally fast FAST
// if you want to write parallel code you don't want to use async {} 
// only use async if you are making call that's orders of magnitude slower (Azure)
// parallel is about throughput, not latency
// async is about concurrency, lots of independent behaviors appearing to
// operate async, runs hundreds of times slower in terms of throughput
// !!!async is mega slow compared to parallel

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
// val ( |Less|Equal|Greater| ) : c:int -> Choice<unit,unit,unit>

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

// This function is elaborated immediately below to return Some(x, xs)
//let (|Char|_|) alphabet chars =
//    match chars with
//    | [] -> None
//    | x::xs when Set.contains x alphabet -> Some x
//    | _ -> None

let (|Char|_|) alphabet chars =
    match chars with
    | [] -> None
    | x::xs when Set.contains x alphabet -> Some(x,xs)
    | _ -> None

// You can't do: let x = Char alpha 'a'
// gives: error FS0039: The value or constructor 'Char' is not defined

// None
match [] with
| Char alpha c -> Some c
| _ -> None

// Match a, but xs will be understandably empty
match ['a'] with 
| Char alpha c -> Some c
| _ -> None

// This will put 'b' in the xs list
match ['a'; 'b'] with 
| Char alpha c -> Some c
| _ -> None

// Fails because 8 isn't in the alphabet
match ['8'] with
| Char alpha c -> Some c
| _ -> None

// Multiple versions of things here
match ['H'; 'e'; 'l'; 'l'; 'o';] with 
| Char alpha c -> Some c
| _ -> None

// Works because |Char|_| isn't going through the entire list
// so it doesn't directly match against 8
match ['h'; 'e'; '8'; 'l'; 'o';] with 
| Char alpha c -> Some c
| _ -> None

// Fails (returns None) because 3 is not in the alphabet
match ['3'; 'e'; 'l'; 'l'; 'o';] with 
| Char alpha c -> Some c
| _ -> None

match ['H'; 'e'; 'l'; 'l'; 'o';] with 
// call our partial active pattern
| Char alpha (c0, Char alpha (c1, cs)) -> Some(c0, c1, cs)
| _ -> None

match ['H'; 'e'; 'l'; 'l'; 'o';] with 
// call our partial active pattern
| Char alpha (c0, Char alpha (c1, Char alpha (c2, cs))) -> Some(c0, c1, c2, cs)
| _ -> None

//////

let rec (|Chars|) alphabet chars =
    match chars with
    | Char alphabet (c, Chars alphabet (cs, xs)) -> (c::cs, xs)
    | xs -> [], xs

// Match alpha characters into cs
match ['H'; 'e'; 'l'; 'l'; 'o'] with 
| Chars alpha (cs, xs) -> cs, xs

// Match numbers into xs
match ['9'; '8'; '1'; '0'; '3'] with 
| Chars alpha (cs, xs) -> cs, xs

// Match alpha and non-alpha characters into their separate lists
match ['A'; 'K'; '4'; '7'] with 
| Chars alpha (cs, xs) -> cs, xs

// But really, the way |Chars| is written, once it fails to match the rest
// of the content of chars ends up in xs regardless of whether it's alpha, 
// numeric or otherwise. See below - where a space character causes the 
// remainder of chars to end up in xs

List.ofSeq "Hello world!"
// ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!']

match List.ofSeq "Hello world!" with
| Chars alpha (cs, xs) -> cs, xs

match ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with 
| Chars alpha (cs, xs) -> cs, xs

// Used to create DSL - whole lexing and parsing is about 100 lines of code
// Automates an enormous amount of work for actuaries to go into production

let (|Ident|_|) chars = 
    match chars with
    | Char alpha (c, Chars alphanum (cs, xs)) -> Some(c::cs, xs)
    | _ -> None

match ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with 
| Ident (cs, xs) -> Some(cs, xs)
| _ -> None

match ['H'; 'e'; 'l'; 'l'; 'o'; '1'; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with 
| Ident (cs, xs) -> Some(cs, xs)
| _ -> None

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

type Vec2_a(x:float, y:float) =
    member this.X = x
    member this.Y = y

Vec2_a(3.0, 4.0)
(Vec2_a(3.0, 4.0))
Vec2_a(3.0, 4.0) = (Vec2_a(3.0, 4.0))
(Vec2_a(3.0, 4.0)).X

type Vec2_b(x:float, y:float) =
    member private this.X = x
    member this.Y = y

(Vec2_b(3.0, 4.0))

type Vec2_c(x:float, y:float) =
    member this.X = x
    member this.Y = y
    member this.Length = sqrt(x*x + y*y)

Vec2_c(3.0, 4.0).Length
(Vec2_c(3.0, 4.0)).Length

// Object Expressions

{ new System.Object() with
    member x.ToString() = "F#!" }

let oe = { new System.Object() with member x.ToString() = "F#!" }
printfn "%s" (oe.ToString())
printfn "%A" oe

// Object Expressions continued
// awesome
do
    use myDisposable =
        { new System.IDisposable with
            member this.Dispose() = printfn "Disposed" }
    ()

open System.IO

let myByte =
    let path = ""
    use stream = File.OpenRead path
    stream.ReadByte()

seq { use stream = File.OpenRead ""
      while true do
        yield stream.ReadByte() }

let readLines file =
    seq { use stream = File.OpenRead ""
          use reader = new StreamReader(stream)
          while not reader.EndOfStream do
            yield stream.ReadByte() }

// implemented above, which was typical code pre-.NET 4.0
File.ReadLines

Directory.GetFiles(".")

///////////////////////////////////////////////////////

// Explicit constructor 
// Type of Class
type Vec2_d(x:float, y:float) =
    member this.X = x
    member this.Y = y
    member this.Length = sqrt(x*x + y*y)

// No explicit constructor
// Type of Record
type Vec2_e = 
    { x:float
      y:float}  
    // use self-identifier to find x and y
    member this.Length = sqrt(this.x * this.x + this.y * this.y)

// 2e
{ x = 3.0; y = 4.0; }.Length
// 2e
let two_e = { x = 3.0; y = 4.0; }
two_e.Length
// 2d
Vec2_d(3.0, 4.0)

type State = 
    | On 
    | Off
    member this.ToInt() =
        match this with
        | On -> 3
        | Off -> 7

On.ToInt()
Off.ToInt()

type Vec3_a = 
    { mutable x: float
      mutable y: float }
    
    // Note: get() and set with argument
    member this.Length 
        with get() = sqrt(this.x * this.x + this.y * this.y)
        and set length =
            let scale = length / this.Length
            this.x <- scale * this.x
            this.y <- scale * this.y

// This is a Vec3_a rather than a Vec2_e because F# chooses the last
// structure with the matching features when attempting to new up 
// a record. 
let u = { x = 3.0; y = 4.0 }

u.Length
u.Length <- 1.0
u

type Vec3_b = 
    { mutable x: float
      mutable y: float }
    
    // Note use of and for mutual definition of get/set
    member this.Length 
        with get() = sqrt(this.x * this.x + this.y * this.y)
        and set length =
            let scale = length / this.Length
            this.x <- scale * this.x
            this.y <- scale * this.y

    member this.Item
        with get i =
            if i = 0 then this.x else this.y
        and set i value =
            if i = 0 then
                this.x <- value
            else
                this.y <- value

//type Vec3_b =
//  {mutable x: float;
//   mutable y: float;}
//  with
//    // What does 'float with get' mean?
//    member Item : i:int -> float with get
//    // Why is this not: member Length : 
//    member Length : float
//    member Item : i:int -> float with set
//    member Length : float with set
//  end

let u_a = { x = 3.0; y = 4.0 }

u_a.[0]
u_a.[1]

u_a.[1] <- -4.0

u_a

////////

type Vec3_c = 
    { mutable x: float
      mutable y: float }
    
    member this.Length 
        with get() = sqrt(this.x * this.x + this.y * this.y)
        and set length =
            let scale = length / this.Length
            this.x <- scale * this.x
            this.y <- scale * this.y

    member this.Item
        with get i =
            if i = 0 then this.x else this.y
        and set i v =
            if i = 0 then
                this.x <- v
            else
                this.y <- v

    static member (+) (u, v) = { x = u.x + v.x; y = u.y + v.y }
    static member (*) (s, v) = { x = s * v.x; y = s * v.y }

let u_b = { x = 3.0; y = 4.0 }
let v = { x = 5.0; y = 9.0 }

u_b + v

2.0 * u_b
2.0 * v

//////////////////////////////////////
// Embedded DSL
// overload plus and multiply etc. operators

type Expr_a =
    | Int of int
    | Var of string
    | Add of Expr_a * Expr_a
    | Sub of Expr_a * Expr_a
    | Mul of Expr_a * Expr_a
    | Num of float

    static member (+) (f, g) = Add(f, g)
    static member (*) (f, g) = Mul(f, g)
    static member (-) (f, g) = Add(f, Mul(Num -1.0, g))

    // he then goes on to completely rewrite the nestish stuff
    // with symbols

// Idea: d (f, "x") // complicated

let expr_a_int = Int(4)
let expr_a_var = Var("foo")
let expr_a_add_manual = Add(Int(4), Int(3))
let expr_a_add_overload = Int(4) + Int(3)
let expr_a_mul_manual = Mul(Int(3), Num(4.5))
let expr_a_mul_overload = Int(3) * Num(4.5)
let expr_a_sub_manual = Sub(Int(4), Int(3))
let expr_a_sub_overload = Int(4) - Int(3)

type Expr_b =
    | Int of int
    | Var of string
    | Add of Expr_b * Expr_b
    | Sub of Expr_b * Expr_b
    | Mul of Expr_b * Expr_b
    | Num of float

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

    // static member (-) (f, g) = Mul(f, Mul(Num -1.0, g))
    static member (-) (f, g) = f + (g * (Int -1))
    // Note, this is an error because '-1' doesn't exist in Expr_b: 
    // static member (-) (f, g) = f + (g * -1)

// Idea: d (f, "x") // way simpler

let expr_b_int = Int(4)
let expr_b_var = Var("foo")
let expr_b_add_manual = Add(Int(4), Int(3))
let expr_b_add_overload = Int(4) + Int(3)
let expr_b_mul_manual = Mul(Int(3), Num(4.5))
let expr_b_mul_overload = Int(3) * Num(4.5)
let expr_b_sub_manual = Sub(Int(4), Int(3))
let expr_b_sub_overload = Int(4) - Int(3)

// ... and in the nest combinator

let rec nest n f x =
    if n = 0 then x else
        nest (n-1) f (f x)
// val nest : n:int -> f:('a -> 'a) -> x:'a -> 'a
// What's the difference between f:('a -> 'a) and x:'a -> 'a

// Examples: 
nest 3 (~-) 1

// or
let add x y = x + y
let addFour = add 4

nest 3 addFour 0

// Example usage of nest wrt Expr_b

let addExpr_b (x:Expr_b) (y:Expr_b) = x + y
let addFour_b = addExpr_b (Int(4))

nest 3 addFour_b (Int(0))

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
