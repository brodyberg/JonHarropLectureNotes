()

// int (System.Int32)
123
234

// float (System.Double)
1.234

// float32 (System.Single)
1.234f

123 + 234
1.23 + 2.34
123 + 2.34
float(123) + 2.34
123 + int(2.34)

float 123 + 2.34
123 + int 2.34

sin(1.23)
sin 1.23

sin 12.3f

1.23M
1.23M + 2.34M

3 = 3
3 < 4
3 <= 4

(0.1 + 0.2) + 0.3 = 0.1 + (0.2 + 0.3)
((0.1 + 0.2) + 0.3) - (0.1 + (0.2 + 0.3))

((0.1M + 0.2M) + 0.3M) - (0.1M + (0.2M + 0.3M))

System.Numerics.Complex(3.0, 4.0)
System.Numerics.Complex(3.0, 4.0).Magnitude

sin(System.Numerics.Complex(3.0, 4.0))

System.Math.Sin

let a = 2
// const int a = 2;
let b = a
let a = a * a
b

let foo = "hello"

let a =
  let b =
    let c = 7
    c+3
  b+2

// indexing
"Hello world".[4]

// conversion
"Hello world"B

2, 3.2

let (a, b) = (2, 3)
let (a, (b, c)) = (2, (3, 4))
let (a, b) = (2, (3, 4))

1, 2, 3
(1, 2), 3
1, (2, 3)

(1, 2, 3) = (1, 2, 3)

(a, b)

type Vector2D = { mutable x: float; y: float }

{ x = 3.0; y = 4.0 } = { x = 3.0; y = 4.0 }
{ x = 0.3; y = 0.4 } = { x = 0.3; y = 0.4 }

printfn "%A" { x = 3.0; y = 4.0 }

printf("%d", 3);

compare { x = 3.0; y = 4.0 } { x = 3.0; y = 4.0 }

{ x = 3.0; y = 4.0 }.GetHashCode()

hash { x = 3.0; y = 4.0 }

[|2.3; 3.4|].GetHashCode()

hash [|2.3; 3.4|]

hash

type Vector3D = { mutable x: float; y: float; z: float }

let u : Vector2D =
  { x = 3.0
    y = 4.0 }

u.x <- 10.0

u

{ u  }

{ x = u.x + 2.0
  y = u.y }

{ u with x = u.x + 2.0 }

f 2
f -2

(*
type State =
  | On of string
  | Off

On "me"
Off

[On "me"; Off]
*)

type BinaryTree =
  | Leaf
  | Branch of BinaryTree * int * BinaryTree

Leaf
Branch(Leaf, 3, Leaf)
Branch(Leaf, 3, Branch(Leaf, 4, Leaf))
Branch(Branch(Leaf, 2, Leaf), 3, Branch(Leaf, 4, Leaf))
let myTree = Branch(Branch(Leaf, 2, Leaf), 3, Branch(Leaf, 4, Leaf))

myTree = myTree
compare myTree myTree
hash myTree

compare Leaf (Branch(Leaf, 3, Leaf))
compare (Branch(Leaf, 2, Leaf)) (Branch(Leaf, 3, Leaf))
compare (Branch(Branch(Leaf, 2, Leaf), 3, Leaf)) (Branch(Leaf, 2, Branch(Leaf, 3, Leaf)))

printfn "%A" (Branch(Leaf, 3, Leaf))

if 3 < 4 then
  printfn "Hello world!"

if 3 < 4 then
  17
else
  33

4.0 * asin(1.0 / sqrt(2.0))

System.Math.PI

(fun (x: float) -> x * x)
(fun x -> x * x : float)

let f x : float = x * x

(fun (x, y) -> (y, x))

let sqr x = x * x

let sqr = (fun x -> x * x)

let swap(a, b) = (b, a)

let func1, func2 = (fun x -> x*x), (fun x -> x+x)

let myPows =
  let sqr x = x * x
  (fun x -> x * sqr x), (fun x -> sqr(sqr x))

let pow3, pow4 = myPows

let applyTwo((f, g), (x, y)) = (f(x), g(y))

applyTwo(myPows, (3, 3))

pow3(3)
pow4(3)

let foo =
  sqr 3.4

let rec pown (x, y) =
  if y=0 then 1.0 else
    x * pown (x, y-1)

pown (2.0, 8)

let rec factorial n =
  if n=0I then 1I else
    n * factorial(n - 1I)

factorial 100I

let swap(x, y) = y, x

type Vector2D<'T> = { x: 'T; y: 'T }
type Point2D<'T> = { x: 'T; y: 'T }

let myVec : Vector2D<_> = { x = 3; y = 4 }

type Option<'a> =
  | None
  | Some of 'a

type BinaryTree<'a> =
  | Leaf
  | Branch of MyBranch<'a>
and MyBranch<'a> =
  { left : BinaryTree<'a>
    elt : 'a
    right : BinaryTree<'a> }

let rec f x = g x
and g x = f x

let swap(x, y) = (y, x)

(x, y)

let choose ((0, x, _) | (_, _, x)) = x

choose(7, 'a', 'b')
choose(0, 'a', 'b')

let def ((x, None) | (_, Some x)) = x

def ('a', None)
def ('a', Some 'b')

let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | p -> fib(p-2) + fib(p-1)

let rec fib = function
  | 0 | 1 as n -> n
  | n -> fib(n-2) + fib(n-1)

let rec fib = function
  | 0 | 1 as n -> abs n
  | n when n<0 -> fib(n+2) + fib(n+1)
  | n -> fib(n-2) + fib(n-1)

fib 1, fib 2, fib 3, fib 4, fib 5

[1;2;3]

let rec pairs = function
  | x0::x1::xs -> (x0, x1)::pairs(x1::xs)
  | [] -> []

pairs [] = []
pairs [3] = []
pairs(1::2::[]) = [1, 2]
pairs [1..5] = [(1, 2); (2, 3); (3, 4); (4, 5)]

let rec pairs = function
  | x0::(x1::_ as xs) -> (x0, x1)::pairs xs
  | [] | [_] -> []

let xs = [1..5]
let ys = pairs xs
let xs = [2..6]
ys

let xs = [ref 2; ref 3; ref 4]

let ys = pairs xs

xs.[0] := 10

ys

type State = On | Off
type Instruction = TurnOn | TurnOff | Spin

let transition state instruction =
  match state, instruction with
  | Off, TurnOn -> On
  | On, TurnOff -> Off
  | state, _ -> state
  //| state, (TurnOn | TurnOff) -> state

let agent =
  MailboxProcessor.Start(fun inbox ->
    let rec loop state =
      async { let! instruction = inbox.Receive()
              return! loop (transition state instruction) }
    loop Off)

agent.Post TurnOn
agent.Post TurnOff

let multiply (a, b) =
  match a, b with
  | 0, _ | _, 0 -> 0
  | a, b -> a * b

let unbox3 (a, b, c) =
  match a, b, c with
  | Some a, Some b, Some c -> Some(a, b, c)
  | _ -> None

unbox3 (Some 1, Some 2, Some 3)

type Expr =
  | Num of float
  | Var of string
  | Add of Expr * Expr
  | Mul of Expr * Expr

// x^3 - x - 1
let f =
  let x = Var "x"
  Add(Add(Mul(Mul(x, x), x), Mul(Num -1.0, x)), Num -1.0)

let rec d (f, x) =
  match f with
  | Var y when x=y -> Num 1.0
  | Num _ | Var _ -> Num 0.0
  | Add(f, g) -> Add(d(f, x), d(g, x))
  | Mul(f, g) -> Add(Mul(f, d(g, x)), Mul(g, d(f, x)))

d (f, "x")

let rec eval (vars, f) =
  match f with
  | Num x -> x
  | Var x -> vars x
  | Add(f, g) -> eval (vars, f) + eval (vars, g)
  | Mul(f, g) -> eval (vars, f) * eval (vars, g)

eval ((fun _ -> 2.0), f)
eval ((fun _ -> 2.0), d (f, "x"))
eval ((fun _ -> 2.0), d (d (f, "x"), "x"))
eval ((fun _ -> 2.0), d (d (d (f, "x"), "x"), "x"))

// 2^3 - 2 - 1

let d (f, x) =
  let dx = 1e-5
  (f(x+dx) - f(x-dx)) / (2.0 * dx)

d((fun x -> x*x*x - x - 1.0), 2.0)

[1; 3]

[1, 2]

[<AllowNullLiteralAttribute>]
type Foo() =
  member __.Value = 3

let mutable x = Foo()

x <- null


[1..5]

[1;2;3]

[|1;2;3|]

seq { 1..5 }
seq { 1.0..1e100 }

seq { 0 .. 3 .. 10 }
seq { 0.0 .. 0.1 .. 1.0 }
seq { 'a' .. 'z' }

let alpha = set['a'..'z'] + set['A'..'Z']
let digit = set['0'..'9']
let alphanum = alpha + digit

Set.contains '3' digit

seq { for i in 1..10 ->
        i * i
      for i in 1..10 ->
        i * i }

seq { for i in 1..10 do
        if i < 5 then
          yield i * i
        yield 3 }

[ for i in 1..10 do
    if i < 5 then
      yield i * i
    yield 3 ]



[ for i in 1..1000 ->
    i * i ]
[|for i in 1..1000 ->
    i * i|]

seq { for i in 1..10 do yield i * i
      for i in 1..10 do yield i * i }

let xs =
  seq { for i in 1..10 do
          yield i * i
        for i in 1..10 do
          yield veryExpensiveCall i
          yield i+1 }

Seq.cache xs

seq { yield 7
      for i in 1..10 do
        yield i * i
      for i in 1..10 do
        yield i * i }


let xs =
  seq { let a = 3
        for i in 1..10 ->
          i * a }

[ for i in 1..8 do
    for j in 1..8 do
      yield i, j ]

[ for i in 1..8 ->
    [ for j in 1..8 ->
        i, j ] ]

exception OhNoes of string
exception OhNoes2 of string

raise(OhNoes "yipes")

try
  raise(System.Collections.Generic.KeyNotFoundException())
  raise(OhNoes "yipes")
  4
with
| OhNoes str -> str.Length
| OhNoes2 str -> 0
| :? System.Collections.Generic.KeyNotFoundException -> -1


/// Very professionally written.
// Terribly badly written.
let a = 3

let f1 x =
  ((x - 1.0) - (x - 1.0) * (x - 1.0)) ** (x - 1.0)

let f2 x =
  let a = x - 1.0
  (a - a * a) ** a

let f (g, x) =
  g(g(x))

f((fun n -> n*n), 5)

let add(m, n) = m + n

let add = fun (m, n) -> m + n
// val add : m:int * n:int -> int

(fun n -> add(n, 2))

add(2, 3) = 5

let add = fun m -> (fun n -> m + n)
// val add : m:int -> (n:int -> int)

(add(2))(3) = 5

add 2

let add m n = m + n

let addOne = add 1

addOne 3

//nest 0 f x = x
//nest 1 f x = f(x)
//nest 2 f x = f(f(x))
//...

let rec nest n f x =
  match n with
  | 0 -> x
  | n -> nest (n-1) f (f x)

let rec nest n f x =
  match n with
  | 0 -> x
  | n -> f(nest (n-1) f x)

let rec even n = odd(n-1)
and odd n = even(n-1)

let evenish odd n = odd(n-1)
let oddish even n = even(n-1)

let rec even n = evenish (oddish even) n

nest 5 (fun str -> str + str) "foo"

nest 100000000 (~-) 5

nest 5 ((*) 2) 1

(fun m n -> m - n)

(fun n -> -n)

(~-)

let rec factorial n =
  if n=0 then 1 else
    n * factorial(n-1)

factorial 1000000

let factorial n =
  let rec loop product n =
    if n=0 then product else
      loop (product * n) (n-1)
  loop 1 n

factorial 1000000



let rec nest n f x =
  match n with
  | 0 -> x
  | n -> nest (n-1) f (f x)

let succ n = n+1

let add m n =
  nest m succ n

add 6 7

let mul m n =
  nest m (add n) 0

mul 6 7

let pow m n =
  nest n (mul m) 1

pow 6 7

pown 6 7

let xs = [1..10]

let rec iter f xs =
  match xs with
  | [] -> ()
  | x::xs ->
      f x
      iter f xs

iter (printfn "%d") xs

List.iter (printfn "%d") xs

for x in xs do
  printfn "%d" x

Set.iter
Map.iter
Array.iter

Seq.iter

let rec map f xs =
  match xs with
  | [] -> []
  | x::xs -> f x::map f xs

map ((+) 1) [1..5]

List.map ((+) 1) [1..5]

[ for x in [1..5] ->
    x + 1 ]

Seq.map ((+) 1) [1..5]

let rec fold f a xs =
  match xs with
  | [] -> a
  | x::xs -> fold f (f a x) xs

fold (+) 0 [1..100]
fold (*) 1 [1..5]
fold max 0 [1..5]

max "2" "3"

List.fold (+) 0 [1..100]

Seq.fold (+) 0 [1..100]

x |> f

f x

[1..10]
|> Seq.map (fun n -> 2*n)
|> Seq.map (fun n -> n*n)
|> Seq.fold (+) 0

[|1..10|]
|> Array.Parallel.map (fun n -> 2*n)
|> Array.Parallel.map (fun n -> n*n)
|> Array.fold (+) 0

[|1..10|]
|> Array.Parallel.map (fun n ->
  let n = 2*n
  n*n)
|> Array.fold (+) 0


[ for n in 1..10 ->
    async { let n = 2*n
            let! data = fetchOverInfiniband url
            return n*n } ]
|> Async.Parallel
|> Async.RunSynchronously
|> Array.fold (+) 0

type Comparison = Less | Equal | Greater

let classify c =
  if c<0 then Less
  elif c=0 then Equal
  else Greater

match classify(compare 3 4) with
| Less -> "less"
| Equal -> "equal"
| Greater -> "greater"

let (|Less|Equal|Greater|) c =
  if c<0 then Less
  elif c=0 then Equal
  else Greater

match compare 3 4 with
| Less -> "less"
| Equal -> "equal"
| Greater -> "greater"

let alpha = set['a'..'z'] + set['A'..'Z']
let digit = set['0'..'9']
let alphanum = alpha + digit

let (|Char|_|) alphabet chars =
  match chars with
  | x::xs when Set.contains x alphabet -> Some(x, xs)
  | _ -> None

match ['H'; 'e'; 'l'; 'l'; 'o'] with
| Char alpha (x, xs) -> x, xs
| _ -> '.', []

match ['H'; 'e'; 'l'; 'l'; 'o'] with
| Char alpha (c0, Char alpha (c1, cs)) -> Some(c0, c1, cs)
| _ -> None

let rec (|Chars|) alphabet chars =
  match chars with
  | Char alphabet (c, Chars alphabet (cs, xs)) ->
      (c::cs, xs)
  | xs -> [], xs

match ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with
| Chars alpha (cs, xs) -> cs, xs

List.ofSeq "Hello world!"

let (|Ident|_|) chars =
  match chars with
  | Char alpha (c, Chars alphanum (cs, xs)) ->
      Some(c::cs, xs)
  | _ -> None

match ['3'; 'e'; 'l'; 'l'; 'o'; '1'; ' '; 'w'; 'o'; 'r'; 'l'; 'd'; '!'] with
| Ident (cs, xs) -> Some(cs, xs)
| _ -> None

// Send your e-mail addresses to:
// jon@ffconsultancy.com






type expr = 
  | Int of int
  | Var of string
  | Apply of expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | Let of bool * string * expr * expr

let alpha = set['a'..'z'] + set['A'..'Z']

let num = set['0'..'9']

let alphanum = alpha + num

let (|Char|_|) alphabet = function
  | c::cs when Set.contains c alphabet -> Some(c, cs)
  | _ -> None

let rec (|Chars|) alphabet = function
  | Char alphabet (c, Chars alphabet (cs, t)) -> c::cs, t
  | t -> [], t

let stringOf (chars: seq<char>) =
  Seq.map string chars
  |> String.concat ""

let (|INT|IDENT|KWD|END|) t =
  let rec ws = function ' '::t -> ws t | t -> t
  match ws t with
  | '-'::Char num (c, Chars num (cs, t)) -> INT(-int(stringOf(c::cs)), t)
  | Char num (c, Chars num (cs, t)) -> INT(int(stringOf(c::cs)), t)
  | Char alpha (c, Chars alphanum (cs, t)) ->
      match stringOf(c::cs) with
      | "if" | "then" | "else"
      | "fun"
      | "let" | "rec" | "in" as s -> KWD(s, t)
      | s -> IDENT(s, t)
  | '-'::'>'::t -> KWD("->", t)
  | ('=' | '(' | ')' as c)::t -> KWD(string c, t)
  | t -> END

let rec (|Atom|_|) = function
  | INT(n, t) -> Some(Int n, t)
  | IDENT(x, t) -> Some(Var x, t)
  | KWD("(", Expr(f, KWD(")", t))) -> Some(f, t)
  | _ -> None
and (|PApply|_|) = function
  | Atom(f, PApply(fs, t)) -> Some(f::fs, t)
  | Atom(f, t) -> Some([f], t)
  | _ -> None
and (|Expr|_|) = function
  | PApply(fs, t) -> Some(List.reduce (fun f x -> Apply(f, x)) fs, t)
  | KWD("if", Expr(p, KWD("then", Expr(f, KWD("else", Expr(g, t)))))) ->
      Some(If(p, f, g), t)
  | KWD("fun", IDENT(x, KWD("->", Expr(f, t)))) ->
      Some(Fun(x, f), t)
  | KWD("let", IDENT(x, KWD("=", Expr(f, KWD("in", Expr(g, t)))))) ->
      Some(Let(false, x, f, g), t)
  | KWD("let", KWD("rec", IDENT(x, KWD("=", Expr(f, KWD("in", Expr(g, t))))))) ->
      Some(Let(true, x, f, g), t)
  | _ -> None

type Vec2(x: float, y: float) =
  member this.X = x
  member this.Y = y
  member this.Length = sqrt(x*x + y*y)

Vec2(3.0, 4.0).Length

{ new System.Object() with
    member this.ToString() = "F#!" }

do
  use myDisposable =
    { new System.IDisposable with
        member this.Dispose() =
          printfn "Disposed" }
  ()

let myByte =
  let path = ""
  use stream = System.IO.File.OpenRead path
  stream.ReadByte()

(System.Uri "http://www.google.com").AbsolutePath

let readLines file =
  seq { use stream = System.IO.File.OpenRead file
        use reader = (new System.IO.StreamReader(stream))
        while not reader.EndOfStream do
          yield reader.ReadLine() }

type Vec2 =
  { x: float
    y: float }

  member this.Length = sqrt(this.x*this.x + this.y*this.y)

{ x = 3.0; y = 4.0 }.Length

type State =
  | On
  | Off

  member this.ToInt() =
    match this with
    | On -> 3
    | Off -> 7

On.ToInt()

type Vec2 =
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
  static member (*) (s, u) = { x = s * u.x; y = s * u.y }

let u = { x = 3.0; y = 4.0 }

u + u
2.0 * u

u.Length

u.Length <- 1.0

u

u.[0]
u.[1]
u.[1] <- -4.0

[|1..10|].[3..5]

type Expr =
  | Num of float
  | Var of string
  | Add of Expr * Expr
  | Mul of Expr * Expr

  static member (+) (f, g) =
    match f, g with
    | Num 0.0, f
    | f, Num 0.0 -> f
    | f, g -> Add(f, g)

  static member (-) (f, g) = f + Num -1.0 * g

  static member (*) (f, g) =
    match f, g with
    | Num 0.0, _
    | _, Num 0.0 -> Num 0.0
    | Num 1.0, f
    | f, Num 1.0 -> f
    | f, g -> Mul(f, g)

// x^3 - x - 1
let f =
  let x = Var "x"
  x*x*x - x - Num 1.0

let rec d x f =
  match f with
  | Var y when x=y -> Num 1.0
  | Num _ | Var _ -> Num 0.0
  | Add(f, g) -> d x f + d x g
  | Mul(f, g) -> f * d x g + g * d x f

let rec expand = function
  | Num _ | Var _ as f -> f
  | Mul(Add(f, g), h)
  | Mul(h, Add(f, g)) -> expand(f * h + g * h)
  | Add(f, g) -> expand f + expand g
  | Mul(f, g) -> expand f * expand g

Seq.iter (printfn "%d") [1..5]
Seq.iter (printfn "%d") [|1..5|]

expand (Var "f" * (Var "g" + Var "h"))

d "x" f

let rec eval (vars, f) =
  match f with
  | Num x -> x
  | Var x -> vars x
  | Add(f, g) -> eval (vars, f) + eval (vars, g)
  | Mul(f, g) -> eval (vars, f) * eval (vars, g)

let rec nest n f x =
  if n=0 then x else
    nest (n-1) f (f x)

eval ((fun _ -> 2.0), nest 3 (d "x") f)

Array.init 10 (fun n -> n)
|> seq

Array2D.init 10 10 (fun i j -> i+j)
|> Array2D.iteri (fun i j x -> ())

Array3D
