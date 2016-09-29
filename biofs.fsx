#I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#load "./Parse.fs"
open Bio.Parse
open Bio.Type
open Bio.Utils
open Bio.deBruijn
#load "./Ba3.fs"
open Ba3.Ba3e
open FParsec
open System.IO


let inp = """0 -> 3
1 -> 0
2 -> 1,6
3 -> 2
4 -> 2
5 -> 4
6 -> 5,8
7 -> 9
8 -> 7
9 -> 6
"""

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let str s = pstring s
let sepList : Parser<_> = between (str "[") (str "]") (sepBy pint32 (str ";"))

let (p: Parser<_> ) = sepBy int (pchar ',')
let pEulCycLine : Parser<_> = int .>> pstring " -> " .>>. (sepBy int (pchar ','))
// run pEulCycLine "14 -> 3"  // Success: (14, [3])
// run pEulCycLine "14 -> 30,23"  // Success: (14, [30; 23])
let pEulCyc = sepEndBy1 pEulCycLine nl
run pEulCyc inp

type Edge<'a> = ('a * 'a)
type EdgeList<'a> = Edge<'a> list
type EdgeSet<'a when 'a: comparison> = Set<Edge<'a>>
type EdgeMap<'a when 'a: comparison> = Map<'a, Set<Edge<'a>>>

let src ((s, _): Edge<'a>): 'a = s
let snk ((_, k): Edge<'a>): 'a  = k
// type EdgeMap<'a,'b when 'a : comparison> = Map<'a, Set<'a>>

//   ('a * 'b) Map
// let m = Map.empty.Add(1, [(1, 2)])

let tupsL2eL (xs: ('a * 'b list) list) : EdgeList<'a> =
    let rec tupL2El = function
        | h, (y :: ys) -> (h, y) :: tupL2El (h, ys)
        | h, [] -> []
    List.collect tupL2El xs

let (Success((rawvals), _, _)) = run (pEulCyc) inp
let (Success((vals), _, _)) = run (pEulCyc |>> tupsL2eL) inp

let tupsL2eM (xs: ('a * 'a list) list): EdgeMap<'a> =
    let tupL2Em (h, ys) =
        (h, (Set.map (fun x -> (h, x)) (Set.ofList ys)))
    xs |> List.map tupL2Em |> Map.ofList

let remove (emap: EdgeMap<'a>) (start, fin): EdgeMap<'a> =
    let edges = emap.Item start
    Map.add start (edges.Remove (start, fin)) emap

let em = tupsL2eM rawvals
let ue = Set.ofList vals
// let res2 = remove res (0, 3)

// res2.Item 1 |> Set.toSeq
// // let selectNode =

let unitePairs xs = Set.union (Set.map fst xs) (Set.map snd xs)
// let allNodes = unitePairs unexploredEdges
// let startNode = List.ofSeq allNodes |> List.min

let chooseNodeGen f edgeSet =
    let allNodes = unitePairs edgeSet
    List.ofSeq allNodes |> f

let chooseNode x = chooseNodeGen List.min x

// let chooseEdge (edgeCands: EdgeSet<'a>) =
let chooseEdge2 (edgeCands: Set<'a>) =
    if edgeCands.IsEmpty then None
     else Some (Set.toList edgeCands |> List.head)
    // | edgeSet.toSeq |> List.ofSeq

let chooseEdge (edgeCands: EdgeSet<'a>): Option<Edge<'a>> =
    match Set.toList edgeCands with
    | y :: _ -> Some y
    | [] -> None

let ct a _ = a
let flip f x y = f y x
let until (f: ('a -> bool)) (ys: 'a list) =
    let rec untilR xs falses =
        match xs with
        | [] -> (falses, [])
        | x :: rst ->
            match f x with
            | true -> (List.rev falses, x :: rst)
            | false -> untilR rst (x :: falses)
    untilR ys []

// let t: (int * int) = (1, 2)
let (><) f a b = f b a
let logx x = if (printfn "\t> %A" x) = () then x else x
// let (<!>) x = logx x

let unrollEmap (emap: EdgeMap<'a>): EdgeSet<'a> = emap |> Map.toSeq |> Seq.map snd |> Seq.reduce Set.union
let cleanMap (em: EdgeMap<'a>) = Map.filter (fun _ y -> y |> Set.isEmpty |> not)  em

let randCycle (emap: EdgeMap<'a>) =
    let startNode = unrollEmap emap |> chooseNode
    let rec randCycleR (em: EdgeMap<'a>) path node =
        match em.TryFind node |> ((><) defaultArg) Set.empty
            |> chooseEdge with
        | Some (edge: Edge<'a>) ->
            randCycleR (remove em edge) (src edge:: path) (snk edge)
        | None -> (em, path)
    let retem, retpath = randCycleR emap [] startNode
    retem, List.rev retpath

let em2, path = randCycle em
let em2Src emap = emap |> Map.toList |> List.collect (snd >> (Set.map fst) >> Set.toList) |> Set.ofList
let unusedSrcs = em2Src em2

let unex, expl = until unusedSrcs.Contains path



// let until f = function
//     | [] -> []
//     | x :: xs ->
//         match f x with
//         | true -> 1

pre, List.partition unusedSrcs.Contains path (* (fun x -> unusedSrcs.Contains)*)
cleanMap em2
path

let ues = em |> Map.toSeq |> Seq.map snd |> Seq.reduce (+)
ues = ue


    // let edgeCands = emap.Item startNode
    // if edgeCands.IsEmpty then (emap, unexploredEdges)
    //     else 1
    // match (emap.Item startNode) with
    // | Set.empty -> 0
    // 2

let s = Seq.ofList [1; 2; 3]
let s = [1; 2; 3]
s.Head
[1].Head
randCycle List.min unexploredEdges

readWrite "data/ch3/dataset_200_7.txt" strandsD (solveDbg)



