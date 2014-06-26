// no good type providers vs. foreign interfaces
// type providers to a general interface are hopeless right now
// but to a specific vendor it's fine, perfectly great

// triple quotes as a string literal
// handles double quotes

// type providers!!!!
// infers fields and types

// json: looks at first thousand rows
// there? or not there? option
// xml does another thing: if you have nested elements in a tree
// you can apply the types down the tree

// asterank - NASA
// pluto, mars!

// then the xml one
// just to get them worked because the type providers are changing(!)

// still, it allows you to walk into the doc types with intellisense

// lol: "it was a right faff" 

// type provider actual later, now collections

// this is a standard .net array that will look the same to
// both c# and f#
let xs = [|1;2;3;|]

Array.init 10 (fun i -> i)

Array.init 10 id

// the identity
let id x = x

Array.init 10 float

// eta reduction
// 1 = x + 1 // cross things off both sides
// can do it with lambda functions as well
// in the absence of side effects if you have 
// fun x -> f x
// you can cancel the x from both sides
// leaving you with: f
// exactly the same

// function that takes an int list and returns an int list
List.map ((+) 1)

List.map ((+) 1) [1..5]

// adds one to each list
// very succinct
List.map (List.map ((+) 1))

List.map (List.map ((+) 1)) [[1..3]; [4..6]; [7..9]]
// aka
List.map (fun row -> List.map (fun n -> n + 1) row)
List.map (List.map ((+) 1))

// above: functional composition and currying starts to pay off bigtime

// visits each and converts to string
// just like above: Array.init 10 float
Array.init 10 string
// note: these are .net arrays so they are mutable!!!

let xs = Array.init 10 string

xs.[5]
xs

//// now with list
let xs = [1;2;3;]

List.init 10 float

let xs = List.init 10 string
xs.[5]
// Propety 'Item' cannot be set: 
// xs.[5] <- "hello"

xs

// how to change the fifth element
// projects list into changed form
List.mapi (fun i x -> if i = 5 then "Hello" else x) xs
// if you do this, you need a different datastructure
// ie. don't use list in this particular case

// see: fsharp_journal
// knights tour
// uses sets rather than lists
// heavy use of back tracking
// ie. a set is a nested scope
// also: recursive calls are namespaces and backtracking while
// you go up and down the recursion
// datastructures super important in logic programming
// wait, what's "logic programming"? sounds like work to me

// aka .NET List<int>
let xs = ResizeArray<int>()

for i in 1..10 do
    xs.Add i

xs.ToArray()

Array.append [|1..3|] [|4..6|]

Array.create 10 0.0
// like init but you give it an element and it puts that element in every 
// point in the array

// A better way to describe 0.0 above is that it is a value that may be
// computed with any function, and is computed exactly once. The below example
// with ref demonstrates the value being computed by (ref 0) exactly once. 

// ref refresher
let x = ref 10
x:=5
!x

let xs = Array.init 10 (fun i -> ref 0) // fun called n times, 1x per cell
let ys = Array.create 10 (ref 0) // executed before array create even called
// so you get only one ref

// difference? as labeled above

xs.[0] := 10
ys.[0] := 10

xs
ys

////////
let xs = [|1..10|]
xs.[1]

// slicing!!!
xs.[1..3]
xs.[1..]
xs.[..3]

Array.iter (fun n -> printfn "%A" n) xs
// eta reduction: cancel n term on both sides to produce: 
Array.iter (printfn "%A") xs

Array.map ((+) 3) xs
// un-reducing: 
Array.map (fun x -> x + 3) xs

// can't cancel if function does something with n
// can cancel if it's on both sides and function is waiting for call

Array.map (fun x -> x < 3) xs
// hard to read: 
Array.map ((>) 3) xs

// mean same thing, but comparison swapped
Array.map (fun x -> x > 3) xs
// but when partially applied we apply the left hand side, so you have to 
// remember that it swaps sides

Seq.zip

// Seq.filter is more about filtering IN values

Seq.filter ((>) 3) xs

///////////////////

// vector algebra
let xs = [|1.0; 2.0; 3.0;|]
let ys = [|2.0; 3.0; 4.0;|]

// how to combine?
Array.map2 (fun x y -> x + y) xs ys
// like a zip and a map all in one

// SAME!!! 
Array.map2 (+) xs ys

// reduced: 
let add xs ys = Array.map2 (+) xs ys
let add = Array.map2 (+)
let plus x y : float = x + y

let add : float [] -> _ = Array.map2 (+)

type Vector = 
    | Vector of float []

    static member (+) (Vector xs, Vector ys) =
        Vector(Array.map2 (+) xs ys)

Vector xs + Vector ys

// no easy eta reduction
Array.fold2 (fun t x y -> t + x * y) 0.0 xs ys
// same thing with eta reduction
Seq.map2(*) xs ys
|> Seq.fold (+) 0.0

// and using built in
Seq.map2(*) xs ys
|> Seq.sum

// awesome!!!!! (above) so small, so concise

(fun n -> n+1)
(fun m n -> m + n)
(+) // same as above
// m and n are NOT A PAIR
// it's a curried function
(fun m -> fun n -> m + n) // means same thing as above

// function that will compute dot product of two vectors
let dot = Array.fold2 (fun t x y -> t + x * y) 0.0
dot xs ys

let xs = [|1;9;2;8;3;7;4;6;5|]
Array.sort xs // NOTE: returns new array
// sortInPlace mutates array
// sortBy projects
Array.sortBy (fun n -> -n) xs
// eta reduced
Array.sortBy (~-) xs

Array.sortWith (fun m n -> compare (m%3) (n%3)) xs

// function that implements sortWith, by allowing you to pass in function
// on which to do comparison
let sortBy f xs =
    Array.sortWith (fun x y -> compare (f x) (f y)) xs
// n log n sort

// pattern match
let cross xs ys : float [] =
    match xs, ys with
    | [|x1; y1; z1|], [|x2; y2; z2|] -> 
        [|y1 * z2 - z1 * y2
          z1 * x2 - x1 * z2
          x1 * y2 - y1 * x2|]
    | _ -> failwith "Oh noes"

// ie. you can match over array literals
// super nice
// in reality: you'd want a 3d vector type so you can be more typeful rather than
// allowing random entries like the free array does

// change types to get rid of runtime exceptions
// replace array with 3-tuple
let cross xs ys : float * float * float =
    match xs, ys with
    | (x1, y1, z1), (x2, y2, z2) -> 
        y1 * z2 - z1 * y2,
        z1 * x2 - x1 * z2,
        x1 * y2 - y1 * x2
    // gets rid of exceptions!
    // so much wow

// problem: 
type Person_a(name:string) = 
    member this.Name
        with get () = name
type Annuitants_a = Person_a list // list type can be empty
// so we need runtime checks for empty list :_(

let brody = Person_a "brody" 
let annuitants = [brody]

// solution: 
type Annuitants_b = Person_a * Person_a list // this complete type pair
// must always have Person filled in
// so AT COMPILE TIME we always have at least one person

type Annuitants_c = Annuitants_c of Person_a * list<Person_a>

Annuitants_c(Person_a("brody"), [])

List.reduce (+)

List.reduce (+) [] // ASSUMES LIST NOT EMPTY : EXCEPTION

type MyList<'a> = MyList of 'a * list<'a>

let reduce f (MyList(x, xs)) =
    List.fold f x xs

reduce (+) (MyList(1, [2..5]))
reduce (+) (MyList(1, []))
// so more typeful means we'll never throw on empty because list type 
// we're using list that can't be empty
// but we'll have to override all list functions etc

seq { yield 1
      yield! [2..5] }

// he did awesome IEnumerable implementation 
// where he does pattern matching to pull things out
// if I ever have a hard time getting things I need to remember
// structural pattern matching in let expressions

/////////////////
// Sets and Maps

let s1 = set[1..5]
let s2 = set[3..7]
let s3 = set[7..10]

Set.union s1 s2
Set.union s2 s3
Set.intersect s1 s2
Set.difference s2 s3
Set.contains 5 s2

// these tend to be EXTREMELY FAST on functional sets
// because: in case of union, you have a tree (both balanced binary search trees)
// representing different sets
// both immutable
// all you do to create union is create a pointer to both trees
// hashsets have to loop through one of the sets
// if sets are completely interspersed you lose
// but when there are small or no interspersion you'll win
// so, set theoretic operations are lightning fast
// if you are doing operations one at a time in a loop use imperative
// but really that's a set theoretic operation, so convert into that
// ie. think in set theoretic operations not individual elements
// big deal

set [1;1;1;2;2;2;2;3]

// shared lineage: 

[1..2]
[3..5]
[6..7]
[8..10]

Set.empty
|> Set.add 2
|> Set.add 5
|> Set.remove 2
|> Set.count

// map similar

Map.empty
|> Map.add 2 "2"
|> Map.add 5 "5"
|> Map.remove 2
|> Map.find 5

// supports 'fast backtracking' 
let d = Map[for k in 1..1000 -> k, string k]

for kv in d do
    printfn "%A" (kv.Key, kv.Value)

for (x, y) in [1, 2; 2, 3] do
    printfn "%A->%A" x y

// built-in active pattern!
for KeyValue(k, v) in d do
    printfn "%A" (k,v)

// sick

// lineage
// all operations log n
// mutable operation order: 1
// but to do lineage you'd have to recreate each dictionary each time
// so, this is hard in mutable maps
let m0 = Map.empty
let m1 = Map.add 2 "2" m0
let m2 = Map.add 3 "3" m1
let m3 = Map.add 5 "5" m2

// and add to another, older map
let m2b = Map.add 6 "6" m1

// as long as lineage is a straight line mutation is fine
// any time we branch, like m2b you need immutability 

// audience question: how readable are Fsharp projects?
// very nice to expose immutable structures over api
// apis are very clear
// actual code that individuals write? people settle quickly on a 
// comprehensible style
// people refactor for readability
// even his old code is comprehensible
// where old C++ is insane
// full of advanced OO tricks
// where fsharp stays simple
// ocaml just massively easier to refactor (thanks to type inference)
// but in c++ minor changes have monstrous implications with c++ template
// errors
// way way easier to refactor and follow
// and you have a debugger
// but he rarely uses it
// just uses a flow of data through functions
// very few surprises

// risk as pointed out by manager of devs in audience: 
// people do in the new what they were good at in the old
// show them: computer algebra, write in OO style and lets 
// compare. they'd be insane to not see it's a huge improvement
// doesn't make everything better, but it's better with things 
// like trees etc

///////// scala?
// fsharp captures productivity benefits of ML due to type inference

let rec factorial n =
    if n=0 then 1 else
        n * factorial (n-1)

// you won't get type inference in c# or scala

let rec nest n f x =
    if n=0 then x else
        nest (n-1) f (f x)

Array.fold2
// you quickly get to the point where super useful functions
// tell you how to use them because you get the signature
// *and* you don't have to write out the types
// in scala functions are often the same size as their type :-(
// and scala felt like everything and the kitchen sink was there
// and the compiler would suffer under the weight
// scala emacs vs. fsharp VS lol
// super academic
// eclipse is the worst
// eclipse mode for scala now :-(
// problems: ecosystem, tooling
// whereas fsharp even as an academic thing it had decent VS mode
// ALSO: he has no legacy in the JVM code
// why is scala adoption higher than fsharp adoption?
// nothing about fsharp, its about what they are running away from
// ie. c# and Java
// but they want to stay on the JVM due to legacy

// what about haskell
// tried in 2007
// tooling even worse than scala
// tons of neat academic things
// hear: why can't fsharp have haskell stuff?
// feels we should stick to basics
// simple is good
// fsharp is that language
// and every grad student ever has something in haskell
// fsharp is a pick and mix
// "higher kinds" is nice but doesn't fit on .net
// and ocaml has no interop story 
// and you need to work with the GC and that's super
// difficult to debug
// same problems with haskell :-(

// so fsharp is industrial strength
// fast
// concise thanks to ML
// good combination of things

/////////////////////
// concurrent programming

// async is all about breaking free from blocking threads
// about 1mb of stack for each thread
// context switching is super expensive
// want to service 1k connections OR MANY ORDERS MORE

// 3m synchronous C++ code assuming thread local
// thread local is bad

// async is how to service large N connections with small N threads

let stream = System.IO.File.OpenRead ""

// new in .NET 4 and 4.5
// 4.5 added a bunch of stuff for databases
// OS has to support this sort of re-entrant programming model
stream.AsyncRead

let file = "/Users/brodyberg/code/FSharpVsAzureTable/FSharpVsAzureTable.sln"

let data =
    use stream = System.IO.File.OpenRead file
    let bytes = Array.create 1024 0uy
    stream.Read(bytes, 0, bytes.Length)
    |> Array.sub bytes 0

let data =
    async { use stream = System.IO.File.OpenRead file
            let bytes = Array.create 1024 0uy
            // for duration of this call we're not blocking a thread
            // so we can service some other client
            // but we do hop from thread to thread and or cores
            // ie. no affinity for cores or caches
            let! length = stream.AsyncRead(bytes, 0, bytes.Length)
            // comes back on a different thread
            return Array.sub bytes 0 length }

// but all we get is an async
Async.RunSynchronously data

Async.Start data

async { let! bytes = data
        printfn "%A" bytes }
|> Async.Start

// note prompt appears out of order

// this is just like how async works in c# 
// with exception of let! etc. 

// mailbox!

let agent =
    MailboxProcessor.Start(fun inbox -> 
        async { while true do
                    let! msg = inbox.Receive()
                    printfn "%d" msg })

let agent =
    MailboxProcessor<int>.Start(fun inbox -> 
        async { while true do
                    let! msg = inbox.Receive()
                    printfn "%A" msg })

// anyone from anywhere can send messages
// it's a state machine
// asynchronously waits for new message
// when message arrives, resume me so I can handle it

agent.Post 5
agent.Post 10

for i in 1..10 do
    agent.Post i

// firing messages from multiple threads?
// messages will be independently ordered, but collectively interleaved

do 
    System.Console.Write "What is your name? "
    stdout.Flush()
    let name = stdin.ReadLine()
    printfn "Hello %s" name

// MAILBOX THING IS A STATEMACHINE
let agent =
    MailboxProcessor.Start(fun inbox -> 
        async { while true do
                    let! msg = inbox.Receive()
                    printfn "%d" msg })

type State = On | Off
type Instruction = 
    | TurnOn 
    | TurnOff 
    | Info//  of string

// doensn't build now
let agent =
    MailboxProcessor.Start(fun inbox -> 
        let rec loop state =
            async { let! instruction = inbox.Receive()
                    match state, instruction with
                    | Off, TurnOn -> return! loop On // understood to be
                    // a tail call and optimized away
                    // so no leaked stack space
                    // do! will crash real nice though
                    | On, TurnOff -> return! loop Off
                    | state, Info reply -> 
                        reply state
                        return! loop state
                    | state, _ -> return! loop state }
        loop Off) // start in Off state

// doensn't build now
let agent =
    MailboxProcessor.Start(fun inbox -> 
        let rec loop state =
            async { let! instruction = inbox.Receive()
                let state =
                    match state, instruction with
                    | Off, TurnOn -> On 
                    | On, TurnOff -> Off
                    | state, Info reply -> 
                        reply state
                        state
                    | state, _ -> state 
                return! loop state }
        loop Off)

// synchronous calls, just for example
agent.PostAndReply(fun reply -> Info reply.Reply)
agent.Post TurnOn
agent.PostAndReply(fun reply -> Info reply.Reply)

// lots of cases when you want a statemachine
// 600 lines of code for the state machine to run 
// the two machines that ran the stock exchange he wrote that sync'd database
// this style makes state difficult to get wrong

// structural serializer is preferred than the .net binary serializer
// less fragile, works better across types and to handle functions so things
// don't croak


open System.Net

let server = Sockets.TcpListener(IPAddress.Any, 6000)
server.Start()
while true do
    let client = server.AcceptTcpClient()
    async { use client = client // say we're going to dispose of client
            use stream = client.GetStream() // we want to dispose
            while true do
                let! bytes = stream.AsyncRead 1 // read one byte at a time
                do! stream.AsyncWrite(bytes, 0, bytes.Length) }
    |> Async.Start

open System.Text.RegularExpressions

let link = Regex("href=\"([^\"]+)")

type Message = 
    | Finished
    | Visit of string

// crawl web pages on the same server or subdirectory
// if that's the case, apply f
// given url as vertex, tell me which are reachable from that
// doesn't build right now
let iterLinks baseUri html f = 
    for url: Match in link.Matches |> Seq.cast do
        let uri = ref null
        if System.Uri.TryCreate(baseUri, url.Groups.[1].Value, uri) then
            if baseUri.IsBaseOf !uri then
                string !uri // because uri is a ref
                |> f

// get this code
let crawl (baseUri: string) =
    let baseUri = System.Uri baseUri
    use box = MailboxProcessor.Start(


// some of the states themselves kick off async workflows that post
// back into our mailbox processor
// and states say : done aka Finished etc

// key to async programming is "fire and forget" 
// no one blocks waiting for each other
// EVERYBODY uses inbox.Post, nobody does PostAndReply 

// tryRecieve has a timeout

// the message processor is the one thing that lives the whole time
// and is the thing we kick off from the outside
// started by crawl url 

// work in the MP is sequential, no thread unsafety etc
// and it uses threadsafe immutable set anyway, not that it matters

// so the MP is utterly simple and sequential
// and the type of work it does is entirely comprehensible because the work
// it generates for itself are handled with states that spin out workers

// concurrent programming occurs because the mailbox processor is simplified
// through the inbox model

// order of returns is unknown (from workers)

// einsteins riddle (change scheduler????)
// distributions easy to express with patterns
// compute permutations
// then permute through it all and find satisfier
// so with a huge list comprehension

// convex hulls
// minheaps
// n-queens
// knights-tour
// all of these might be change scheduler things


/// day 2 after lunch

Seq.fold (+) 0 [1..10]
Seq.scan (+) 0 [1..10] // you get all the intermediate values

// why are we talking about the graph thingy?


// having to know thread details is a hassle
// so he creates a "thread-safe shim" over WPF
// which ensures things are only ever created on the UI thread
// which to him seems like a detail we shouldn't have to worry about

////////////////////////////////////
// type provider

// install with nuget: FSharp.Data
// Deedle, SQL 
// need #I to reference where it lives on disk... 
// then #r
// then open

// generates static types on the fly
// can use list comprehension to pull in data into a new format

// FSharp.Charting - Windows Forms lol

// simple: generate all the types you'll need at once
// more: generate more static types on the fly

// here it's eating csv, 
// why can't you feed it java and it give syou pinvoke to that transparently
// ie. consume static types on another static type language and provide interop
// existing lang providers are over dynamic languages
// consults the docs!!!!!!!!!!!!!!!!!!!!!!!

// at least with Java you'd have type information
// and same with xml, by using the xsd schema

// idea: azure type provider for: table, blob, whatever




[<Literal>]
let me = "me"

// he has a tough time thinking about data exploration?

// k-means clustering
// keep iterating until your centroids no longer move
// could we match machines like this? with false positives but generally right on?
// how do we find "minimum distance" from point to centroid?

// wow, awesome
Seq.groupBy (fun n -> n%2=0) [1..10]

Seq.groupBy (fun n -> n%2) [1..10]
// wow
Seq.groupBy (fun n -> n%3) [1..10]

// so groupBy nearest centroid

// how to scale k-means to a massive dataset
// are operations going to run quickly enough?
// is there anything you can do to parallelize them?
// parallelism gets complicated quickly
// lot depends on how long a single iteration takes
// you need to watch out for cache thrashing
// how do you become aware of losing affinity?
// how do you become aware of cache thrashing?
// use message passing between cores about points 
// moving between centroids? using tasks if he could
// so he could get the general child task affinity
// benefits you get in .net 
// parallel speed is all about memory hierarchy not
// about the # of flops 

// principle component analysis

// inline
// does inline
// but has big effect on types and boxing
// really good for arithmetic types

// enumerables are slower than a for i=0 loop

// 10X SLOWER to use enumerable than for i loop
// but not so much slower as to not be usable - more like 
// if you are slow do profiling


// what is a covariance matrix?


// best: 
// this
// that

// worst: 


// conclusion?
// fsharp is for real
// skills are real
// what we can do is real
// start small on a big project and replace small parts as you learn
// shitload of math 
// lots of things to watch out for with respect to performance

// Call Graph Visualizer reads through F# code to figure out
// who calls what

// fsharp interactive thing that lets you save your state
// fsharp compiler service - aims to solve a lot of the usability problems
// gives single coherent interface to all the stuff you see in VS
// idea: web, enter code, server evaluates, sends back to web, even graphical
// rename refactoring
// all being build on the fsharp compiler service


// he does most work in fsx and then splits it into two parts for production
// fsx may get thrown away - so it's fine that it wasn't tested
// team more valuable to have unit tests

// custom test harnesses (Aviva) 
// test result visualization as a colored grid
// and if you know the grid x and y it conveys a lot of information
// so if the output is garbage it will kill all the tests relying on that
// if the row is tests it could be something about the test code
// ie. visualize your test results in interesting ways

// add visual representation in fsharpi
// super useful idea for rotating a red-black tree for example





// fsharp signature file: 
    // must go first!!!!
    // full of: 
    // val fib : int -> int

// fs files
    // then
    // let rec fib n =
        // if ....
    
// now the fsi is a description of the interface to that module
// can put type definitions in fsi file too
// and your intellisense docs there
// so your fs file is cleaner
// it's a different way to do private/public/internal
// or you can use this thing

// so external observer only references fsi file can't see random lets
// fsi is good for exposing an interface to other fsharp programs
// but you have to put type definitions in two places (fsi and fs) 
// which is rough particularly as files get bigger
// and errors about type definition problems are bogus
// so it's a hassle until the last moment

// blagh

// delete anything past to make an abstract type so you don't expose guts
// type 'a PairingHeap // = asdf;ljsadfl;kjsadf deleted

// pro-tip: compilation attributes on classes across fsi and fs must match
// but not on abstract types

// fsi 

// both fs and fsi have same module directive

type PairingHeap<'a> =
    private
    | E
    | T of 'a * 'a PairingHeap list

// E and T private, PairingHeap public

let foo = lazy 4 // don't evaluate just yet
// threadsafe

foo.Value

foo

// just like .net lazy 

// cache obvlivious multhread 
// fftw.org/~athena/papers/tocs08.pdf

// uses default mutable array to synchronize which he thinks is important
// because otherwise you pay the immutable 10x slowdown cost

// spawn just like making a .net task
// sync is like task .result

//Cilk is a dialect of C
// totally different notion of stack 
// they added only four things to the language which rocked the 
// performance in parallel
// what is the uniform grid under them?
// what about a graph algorithm? how do you generalize that?
// no solution yet
// graphlab? cmu -> uw machine learning on graphs

// "cache complexity" is the formal amount of data that needs to be shuffled 
// around to other cores during an algorithm

// convex hulls
// computational geometry analog of quicksort
// need to ask about that

// n queens
// a bit like a fold over all the solutions
// how is this like machine matching? where a "run" is like a queen
// that interferes with other runs
// you place queens/runs and ensure they don't interfere
// queens/runs are removed as they are completed and we try to place more
// filter out all the safe, non-attacked positions after you place each
// queen

// this algorithm throws away the position of the queens but that for us
// would be resources on which the queen controls ie. runs

// what would he suggest about the search?

// and he converts to seq { } to yield! entries in the search result
// so he got rid of the fold and replaced it with a seq { ... } 

// how's performance?



/////////////////// 
// concurrent programming
// ie. servers talking to servers
// look up the language features

