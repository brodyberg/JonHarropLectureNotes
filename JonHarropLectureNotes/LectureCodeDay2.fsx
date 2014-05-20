let xs = [|1;2;3|]

Array.init 10 float

let xs = Array.init 10 string

xs.[5] <- "Hello"

xs

List.map (List.map ((+) 1)) [[1..3]; [4..6]; [7..9]]

List.map (fun row -> List.map (fun n -> n + 1) row)
List.map (List.map ((+) 1))


let xs = [1;2;3]

List.init 10 float

let xs = List.init 10 string

xs.[5]

List.mapi (fun i x -> if i=5 then "Hello" else x) xs

xs

let xs = ResizeArray<int>()

for i in 1..10 do
  xs.Add i

xs.ToArray()

Array.append [|1..3|] [|4..6|]

Array.create 10 0.0

let x = ref 10

x := 5

!x

let xs = Array.init 10 (fun _ -> ref 0)
let ys = Array.create 10 (ref 0)

xs.[0] := 10
ys.[0] := 10

xs
ys

let xs = [|1..10|]

xs.[1]

xs.[1..3]
xs.[1..]
xs.[..3]

Array.iter (printfn "%d") xs

Array.map ((+) 3) xs

Array.map (fun x -> x + 3) xs

Array.map (fun x -> x < 3) xs
let xs = Array.map (fun x -> 3 > x) xs

xs.[true]

Array.map ((>) 3) xs

let isPrime =
  let primes = set[|2;3;5;7;11;13|]
  [|for i in 0..14 ->
      primes.Contains i|]

isPrime
|> Seq.zip [0..14]
|> Seq.choose (fun (i, isPrime) -> if isPrime then Some i else None)
//|> Seq.mapi (fun i isPrime -> i, isPrime)
//|> Seq.filter (fun (i, isPrime) -> isPrime)
//|> Seq.map (fun (i, _) -> i)

Seq.zip [1;2;3] ['a';'b';'c';'d']
List.zip [1;2;3] ['a';'b';'c';'d']

[|for i in 0..isPrime.Length-1 do
    if isPrime.[i] then
      yield i|]

let xs = [|1.0; 2.0; 3.0|]
let ys = [|2.0; 3.0; 4.0|]

let plus x y : float = x + y
let add (xs: float []) ys = Array.map2 (+) xs ys
let add = Array.map2 plus

type Vector =
  | Vector of float []

  static member (+) (Vector xs, Vector ys) =
    Vector(Array.map2 (+) xs ys)

Vector xs + Vector ys

Array.fold2 (fun t x y -> t + x*y) 0.0 xs ys

let dot = Array.fold2 (fun t x y -> t + x*y) 0.0

Seq.map2 (*) xs ys
|> Seq.fold (+) 0.0

Seq.map2 (*) xs ys
|> Seq.sum

(fun m n -> m + n)
(fun m -> fun n -> m + n)

let xs = [|1;9;2;8;3;7;4;6;5|]

Array.sort xs
Array.sortBy (~-) xs
Array.sortWith (fun m n -> compare (m%3) (n%3)) xs

let sortBy f xs =
  Array.sortWith (fun x y -> compare (f x) (f y)) xs

let cross xs ys : float [] =
  match xs, ys with
  | [|x1; y1; z1|], [|x2; y2; z2|] ->
      [|y1 * z2 - z1 * y2
        z1 * x2 - x1 * z2
        x1 * y2 - y1 * x2|]
  | _ -> failwith "Oh noes"

let cross xs ys : float * float * float =
  match xs, ys with
  | (x1, y1, z1), (x2, y2, z2) ->
      y1 * z2 - z1 * y2,
      z1 * x2 - x1 * z2,
      x1 * y2 - y1 * x2

type Person =
  | Person of string

type Annuitants =
  | Annuitants of Person * list<Person>

Annuitants(Person "me!", [])


type Annuitants =
  { FirstAnnuitant: Person
    OtherAnnuitants: list<Person> }

List.reduce (+) []

type MyList<'a> =
  { First: 'a
    Rest: 'a list }

  interface System.Collections.IEnumerable with
    member this.GetEnumerator() =
      upcast (seq this).GetEnumerator()

  interface System.Collections.Generic.IEnumerable<'a> with
    member this.GetEnumerator() =
      let mySeq =
        seq { yield this.First
              yield! this.Rest }
      mySeq.GetEnumerator()

MyList(1, [2..5])
|> Seq.reduce (+)

let reduce f (MyList(x, xs)) =
  List.fold f x xs

reduce (+) (MyList(1, [2..5]))
reduce (+) (MyList(1, []))

let s1 = set[1..5]
let s2 = set[3..7]
let s3 = set[7..10]

Set.union s1 s2
Set.union s2 s3
Set.intersect s1 s2
Set.difference s2 s3

Set.contains 5 s2

set [1;1;1;2;2;3]

Set.empty
|> Set.add 2
|> Set.add 5
|> Set.remove 2
|> Set.count

Map.empty
|> Map.add 2 "2"
|> Map.add 5 "5"
|> Map.remove 2
|> Map.find 5

let d = Map[for k in 1..1000 -> k, string k]

for (x, y) in [1, 2; 2, 3] do
  printfn "%A -> %A" x y

for KeyValue(k, v) in d do
  printfn "%A" (k, v)

let m0 = Map.empty
let m1 = Map.add 2 "2" m0
let m2 = Map.add 3 "3" m1
let m3 = Map.add 5 "5" m2

let m2b = Map.add 6 "6" m1

m0, m1, m2, m2b, m3

let m1b = Map.add 2 "20" m0

let rec factorial n =
  if n=0 then 1 else
    n * factorial(n-1)

let rec nest n f x =
  if n=0 then x else
    nest (n-1) f (f x)

Array.fold2

let file = @"C:\Users\Jon\Documents\lena.gif"

let data =
  use stream = System.IO.File.OpenRead file
  let bytes = Array.create 1024 0uy
  stream.Read(bytes, 0, bytes.Length)
  |> Array.sub bytes 0

let data =
  async { use stream = System.IO.File.OpenRead file
          let bytes = Array.create 1024 0uy
          let! length = stream.AsyncRead(bytes, 0, bytes.Length)
          do! stream.AsyncWrite(bytes, 0, length)
          return Array.sub bytes 0 length }

async { let! bytes = data
        printfn "%A" bytes }
|> Async.Start

let agent =
  MailboxProcessor.Start(fun inbox ->
    async { while true do
              let! msg = inbox.Receive()
              printfn "%d" msg })

for i in 1..10 do
  agent.Post i

type State = On | Off

type Instruction =
  | TurnOn
  | TurnOff
  | Info of (State -> unit)

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

agent.PostAndReply(fun reply -> Info reply.Reply)
agent.Post TurnOn
agent.PostAndReply(fun reply -> Info reply.Reply)


open System.Net

let server = Sockets.TcpListener(IPAddress.Any, 6000)
server.Start()
while true do
  let client = server.AcceptTcpClient()
  async { use client = client
          use stream = client.GetStream()
          while true do
            let! bytes = stream.AsyncRead 4096
            do! stream.AsyncWrite(bytes, 0, bytes.Length) }
  |> Async.Start



open System.Text.RegularExpressions

let link = Regex("href=\"([^\"]+)")

type Message =
  | Finished
  | Visit of string

let iterLinks baseUri html f =
  for url: Match in link.Matches html |> Seq.cast do
    let uri = ref null
    if System.Uri.TryCreate(baseUri, url.Groups.[1].Value, uri) then
      if baseUri.IsBaseOf !uri then
        string !uri
        |> f

let crawl (baseUri: string) =
  let baseUri = System.Uri baseUri
  use box = MailboxProcessor.Start(fun inbox ->
    let rec loop n visited (reply, msg) = async {
      let n, visited =
        match msg with
        | Finished -> n-1, visited
        | Visit uri when Set.contains uri visited -> n, visited
        | Visit uri ->
            async { try
                      use client = new System.Net.WebClient()
                      let! html = client.AsyncDownloadString baseUri
                      iterLinks baseUri html (fun uri -> inbox.Post(reply, Visit uri))
                    finally
                      inbox.Post(reply, Finished) }
            |> Async.Start
            n+1, Set.add uri visited
      let! msg = inbox.TryReceive(0)
      match n, msg with
      | 0, None -> reply visited
      | n, None ->
          let! msg = inbox.Receive()
          do! loop n visited msg
      | n, Some msg ->
          do! loop n visited msg }
    async { let! msg = inbox.Receive()
            do! loop 0 Set.empty msg })
  box.PostAndReply(fun reply -> reply.Reply, Visit(string baseUri))

let urls = crawl "http://w3schools.com/" // 104 104

urls.Count


let rec nest n f x =
  let f_x = f x
  if f_x=x || n=0 then x else nest (n-1) f f_x

let random (data: _ [] []) k =
  let rand = System.Random().NextDouble
  let gen i _ =
    let xs = data |> Seq.map (fun u -> u.[i])
    let x = rand()
    x * Seq.min xs + (1.0 - x) * Seq.max xs
  Array.init k (fun _ -> Array.mapi gen data.[0])

Seq.groupBy (fun n -> n%3) [1..10]

let kmeans (data: _ [] []) distance k =
  let nearestCentroid centroids u = Array.minBy (distance u) centroids
  let iteration (centroids: float [] []) =
    [|for _, us in Seq.groupBy (nearestCentroid centroids) data do
        let n = Seq.length us
        if n <> 0 then
          yield Array.mapi (fun i _ -> us |> Seq.averageBy (fun u -> u.[i])) data.[0]
        else
          yield (random data 1).[0]|]
  random data k
  |> nest 100 iteration
  |> fun centroids -> Seq.groupBy (nearestCentroid centroids) data
(*
val kmeans :
  data:float [] [] ->
    distance:(float [] -> float [] -> 'a) ->
      k:int -> seq<float [] * seq<float []>> when 'a : comparison
*)


#r "FSharpForNumerics"

open Microsoft.FSharp.Math
open FlyingFrog.LinearAlgebra

let inline f x = x + x

f 3
f 3.5

let inline sum n f =
  seq { 0 .. n-1 }
  |> Seq.fold (fun t x -> t + f x) 0.0

sum 10000000 (fun x -> float x)

let inline sum n f =
  let mutable x = 0.0
  for i=0 to n-1 do
    x <- x + f i
  x

sum 10 float

let inline mean n f =
  sum n f / float n

mean 10 float

let transpose (a: float [,]) =
  let nRows, nCols = a.GetLength 0, a.GetLength 1
  Array2D.init nCols nRows (fun i j -> a.[j,i])

let a =
  array2D
    [ [ 1.0; 2.0; 3.0 ]
      [ 0.0; 3.0; 4.0 ]
      [ 0.0; 0.0; 5.0 ] ]

transpose a

let scale s (a: float [,]) =
  Array2D.map ((*) s) a

scale 2.0 a

let mul (a: float [,]) (b: float [,]) =
  let m, n, o = a.GetLength 0, a.GetLength 1, b.GetLength 1
  if n <> b.GetLength 0 then
    failwith "Professional error message"
  Array2D.init m o (fun i k ->
    sum n (fun j -> a.[i,j] * b.[j,k]))

mul a a

let covarianceMatrix (a: float [,]) =
  let nRows, nCols = a.GetLength 0, a.GetLength 1
  let mean = Array.init nCols (fun j -> mean nRows (fun i -> a.[i,j]))
  let a = Array2D.init nRows nCols (fun i j -> a.[i, j] - mean.[j])
  scale (1.0 / float(nCols - 1)) (mul (transpose a) a)

array2D
  [ [-1.0; 1.0; 2.0]
    [-2.0; 3.0; 1.0]
    [4.0; 0.0; 3.0]]
|> covarianceMatrix

let normalize (u: float []) =
  let norm2 = Array.fold (fun t x -> t + x*x) 0.0 u
  Array.map ((*) (1.0 / sqrt norm2)) u

normalize [|3.0; 4.0|]

let rec nest n f x =
  if n=0 then x else
    nest (n-1) f (f x)

let transform (a: float [,]) (u: float []) =
  let m, n = a.GetLength 0, a.GetLength 1
  Array.init m (fun i ->
    sum n (fun j -> a.[i,j] * u.[j]))

transform (array2D [|[|0.0; 1.0|]; [|-1.0; 0.0|]|]) [|2.0; 3.0|]

let principalComponent (a: float [,]) =
  let nCols = a.GetLength 1
  let m = covarianceMatrix a
  let x = Array.init nCols (function 0 -> 1.0 | _ -> 0.0)
  nest 10 (fun x -> normalize(transform m x)) x

#I "../packages/FSharp.Data.2.0.8/lib/net40"
#r "FSharp.Data"

open FSharp.Data

type Iris = CsvProvider<"http://aima.cs.berkeley.edu/data/iris.csv", HasHeaders=false>
let iris = Iris.Load "http://aima.cs.berkeley.edu/data/iris.csv"

iris.Headers

[ for row in iris.Rows ->
    [|row.Column1; row.Column2; row.Column3; row.Column4|]
    |> Array.map float ]

let a =
  array2D
    [ for row in iris.Rows ->
        [ row.Column1; row.Column2; row.Column3; row.Column4]
        |> List.map float ]

let pc0 = principalComponent a

let pc1 =
  let nRows, nCols = a.GetLength 0, a.GetLength 1
  [ for i in 0..nRows-1 ->
      let row j = a.[i, j]
      let s = sum nCols (fun j -> row j * pc0.[j])
      Array.init nCols (fun i -> row i - s * pc0.[i]) ]
  |> array2D
  |> principalComponent

let dot = Array.fold2 (fun t x y -> t + x*y) 0.0

dot pc0 pc1

#r "FSharpForVisualization"

open FlyingFrog.FSharpForVisualization
open FlyingFrog.Graphics

let data =
  Array.init nRows (fun i ->
    let row = Array.init nCols (fun j -> a.[i,j])
    dot row pc0, dot row pc1)

plot data



let safe (x1, y1) (x2, y2) =
  x1 <> x2 && y1 <> y2 && x2 - x1 <> y2 - y1 && x1 - y2 <> x2 - y1

let rec search n qs ps =
  seq { match ps with
        | [] when List.length qs = n -> yield qs
        | [] -> ()
        | q::ps ->
            yield! search n qs ps
            yield! search n (q::qs) (List.filter (safe q) ps) }

let solutions n =
  let ps =
    [ for x in 1..n do
        for y in 1..n do
          yield x, y ]
  search n [] ps

[ for n in 1..9 ->
    solutions n
    |> Seq.length ]

let rec search f n qs ps =
  match ps with
  | [] when List.length qs = n -> f qs
  | [] -> ()
  | q::ps ->
      search f n qs ps
      search f n (q::qs) (List.filter (safe q) ps)

let solutions n =
  let ps =
    [ for x in 1..n do
        for y in 1..n do
          yield x, y ]
  let solns = ResizeArray()
  search (fun qs -> solns.Add qs) n [] ps
  solns.ToArray()

[ for n in 1..9 ->
    solutions n
    |> Seq.length ]

List.map solutions [1..8]


#I "../packages/FSharp.Data.2.0.8/lib/net40"
#r "FSharp.Data"

open FSharp.Data

type MSFT = CsvProvider<"http://ichart.finance.yahoo.com/table.csv?s=MSFT">
let msft = MSFT.Load "http://ichart.finance.yahoo.com/table.csv?s=MSFT"

msft.Headers

[ for row in msft.Rows ->
    row.Open, row.High, row.Low, row.Close ]

// Install-Package FSharp.Charting

#I "../packages/FSharp.Charting.0.90.6/lib/net40"
#r "FSharp.Charting"
#r "System.Windows.Forms.DataVisualization"

let recent =
  [ for row in msft.Rows ->
      row.Date, row.High, row.Low, row.Open, row.Close ]
  |> List.sort

open FSharp.Charting

Chart.Candlestick(recent).WithYAxis(Min = 0.0, Max = 200.0).ShowChart()


#I "../packages/FSharp.Data.2.0.8/lib/net40"
#r "FSharp.Data"

open FSharp.Data

type Simple = JsonProvider<""" { "name":"Thomas", "age":6 } """>
let simple = Simple.Parse(""" { "name":"Jon", "age":36 } """)
simple.Age, simple.Name


type SkyMorph = JsonProvider<"http://www.asterank.com/api/skymorph/search?target=pluto">
let pluto = SkyMorph.Load "http://www.asterank.com/api/skymorph/search?target=pluto"
let ceres = SkyMorph.Load "http://www.asterank.com/api/skymorph/search?target=ceres"

[ for result in pluto.Results ->
    result.Key ]

let imageUrl key =
  "http://asterank.com/api/skymorph/image?key=" + key

open System.Windows.Forms

let pictureBox =
  let form = new Form()
  let pic = new PictureBox()
  pic.Dock <- DockStyle.Fill
  pic.Load(imageUrl pluto.Results.[0].Key)
  form.Controls.Add pic
  form.TopMost <- true
  form.Show()
  pic

async { for result in pluto.Results do
          pictureBox.Load(imageUrl result.Key) }
|> Async.RunSynchronously

for result in ceres.Results do
  System.Threading.Thread.Sleep 1000
  pictureBox.Load(imageUrl result.Key)

async { for result in ceres.Results do
          do! Async.Sleep 1000
          pictureBox.Load(imageUrl result.Key) }
|> Async.RunSynchronously


// Install-Package FSharp.Data

#I "../packages/FSharp.Data.2.0.8/lib/net40"
#r "FSharp.Data"
#r "System.Xml.Linq"

open FSharp.Data

type Haemopexin = XmlProvider<"http://www.rcsb.org/pdb/files/1HXN.xml">
let haemopexin = Haemopexin.Load "http://www.rcsb.org/pdb/files/1HXN.xml"

// Amino acid sequence:
[ for x in haemopexin.PdbxPolySeqSchemeCategory.PdbxPolySeqSchemes ->
    x.MonId ]

// 3D coordinates of atoms:
[ for site in haemopexin.AtomSiteCategory.AtomSites ->
    site.CartnX, site.CartnY, site.CartnZ ]
