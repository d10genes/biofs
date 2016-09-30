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


// let inp = """0 -> 3
// 1 -> 0
// 2 -> 1,6
// 3 -> 2
// 4 -> 2
// 5 -> 4
// 6 -> 5,8
// 7 -> 9
// 8 -> 7
// 9 -> 6
// """

let inp2 = """0 -> 3
3 -> 2
2 -> 1,4
1 -> 0,5
4 -> 2
5 -> 1
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
// run pEulCyc inp

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

// let (Success((rawvals), _, _)) = run (pEulCyc) inp
let (Success((rawvals2), _, _)) = run (pEulCyc) inp2
// let (Success((vals), _, _)) = run (pEulCyc |>> tupsL2eL) inp

let tupsL2eM (xs: ('a * 'a list) list): EdgeMap<'a> =
    let tupL2Em (h, ys) =
        (h, (Set.map (fun x -> (h, x)) (Set.ofList ys)))
    xs |> List.map tupL2Em |> Map.ofList

let remove (emap: EdgeMap<'a>) (start, fin): EdgeMap<'a> =
    let edges = emap.Item start
    Map.add start (edges.Remove (start, fin)) emap

// let ue = Set.ofList vals
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

/// Splits list into 2, where first element that satisfies
/// predicate `f` starts the second list
let until (f: ('a -> bool)) (ys: 'a list) =
    let rec untilR falses xs =
        match xs with
        | [] -> (falses, None, [])
        | x :: rst ->
            match f x with
            | true -> (List.rev falses, Some x, rst)
            | false -> untilR (x :: falses) rst
    untilR [] ys

until ((<=) 3) [1..4]

// let t: (int * int) = (1, 2)
let (><) f a b = f b a
let logx x = if (printfn "\t> %A" x) = () then x else x
// let (<!>) x = logx x

let unrollEmap (emap: EdgeMap<'a>): EdgeSet<'a> = emap |> Map.toSeq |> Seq.map snd |> Seq.reduce Set.union

/// Clear keys with corresponding empty sets.
let cleanMap (em: EdgeMap<'a>): EdgeMap<'a> = Map.filter (fun _ y -> y |> Set.isEmpty |> not)  em

/// Extract all sources from edges in map values.
let em2Src (emap: EdgeMap<'a>): Set<'a> =
    emap |> Map.toList |> List.collect (snd >> (Set.map fst) >> Set.toList) |> Set.ofList


let randCycle (start: 'a option) (emap: EdgeMap<'a>) =
    let startNode = defaultArg start (unrollEmap emap |> chooseNode)
    let rec randCycleR (em: EdgeMap<'a>) path node =
        match em.TryFind node |> ((><) defaultArg) Set.empty
            |> chooseEdge with
        | Some (edge: Edge<'a>) ->
            randCycleR (remove em edge) (src edge:: path) (snk edge)
        | None -> (em, node :: path)
    let retem, retpath = randCycleR emap [] startNode
    retem, List.rev retpath


let eulerCycle (emap_: EdgeMap<'a>) =
    let rec eulerCycleR startM emap before after =
        let emap', path = randCycle startM emap
        let explored, unusedNode, unseen = until (em2Src emap').Contains path // (path |> logx)
        let _, _, _ = (logx explored), (logx unusedNode), (logx unseen)
        match unusedNode with
            // |> logx with
        | (Some n) -> eulerCycleR (Some n) emap' (before @ explored) (unseen @ after)
        | None ->
            let retPath = before @ path @ after
            if emap' |> unrollEmap |> Set.isEmpty
                then retPath, emap'
                else retPath, emap'
                // else eulerCycleR (logx None) emap' before (path @ after)

    eulerCycleR None emap_ [] []

let em = tupsL2eM rawvals2
let p_, e_ = eulerCycle em
unrollEmap e_

// [0; 3; 2; 4; 2; 1; 0]
// [0; 3; 1; 5; 1; 2; 4; 2; 1; 0]

// let em1, path1 = randCycle None em
// let unusedSrcs1 = em2Src em1
// let unex1, expl1::rst1 = until unusedSrcs1.Contains path1

// let em2, path2 = randCycle (Some expl1) em1
// let unusedSrcs2 = em2Src em2
// let unex2, expl2::rst2 = until unusedSrcs2.Contains path1

let ues = em |> Map.toSeq |> Seq.map snd |> Seq.reduce (+)

readWrite "data/ch3/dataset_200_7.txt" strandsD (solveDbg)



