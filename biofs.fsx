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

let (p: Parser<int32 list, 'a> ) = sepBy int (pchar ',')
let pEulCycLine : Parser<_> = int .>> pstring " -> " .>>. (sepBy int (pchar ','))
// run pEulCycLine "14 -> 3"  // Success: (14, [3])
// run pEulCycLine "14 -> 30,23"  // Success: (14, [30; 23])
let pEulCyc = sepEndBy1 pEulCycLine nl
run pEulCyc inp

type Edge<'a,'b> = ('a * 'b)
type EdgeList<'a,'b> = Edge<'a,'b> list
type EdgeSet<'a, 'b  when 'a: comparison and 'b: comparison> = Set<Edge<'a,'b>>
type EdgeMap<'a, 'b  when 'a: comparison and 'b: comparison> = Map<'a, Set<Edge<'a,'b>>>
// type EdgeMap<'a,'b when 'a : comparison> = Map<'a, Set<'a,'b>>

let (Success((rawvals), _, _)) = run (pEulCyc) inp
let (Success((vals), _, _)) = run (pEulCyc |>> tupsL2eL) inp

//   ('a * 'b) Map
let m = Map.empty.Add(1, [(1, 2)])

// : EdgeList<'a,'b>
let tupsL2eL (xs: ('a * 'b list) list) : EdgeList<'a,'b> =
    let rec tupL2El = function
        | h, (y :: ys) -> (h, y) :: tupL2El (h, ys)
        | h, [] -> []
    List.collect tupL2El xs

let tupsL2eM (xs: ('a * 'b list) list): EdgeMap<'a, 'b> =
    let tupL2Em (h, ys) =
        (h, (Set.map (fun x -> (h, x)) (Set.ofList ys)))
    xs |> List.map tupL2Em |> Map.ofList

let remove (emap: EdgeMap<'a, 'b>) (start, fin): EdgeMap<'a, 'b> =
    let edges = emap.Item start

    Map.add start (edges.Remove (start, fin)) emap

let res = tupsL2eM rawvals
let res2 = remove res (0, 3)

let unexploredEdges = Set.ofList vals
// let selectNode =
let unitePairs xs = Set.union (Set.map fst xs) (Set.map snd xs)
let allNodes = unitePairs unexploredEdges
let startNode = List.ofSeq  allNodes |> List.min

let chooseNode f edgeSet =
    let allNodes = unitePairs edgeSet
    List.ofSeq  allNodes |> f

chooseNode List.min unexploredEdges

let randCycle f unexploredEdges =
    let startNode = chooseNode f unexploredEdges


randCycle List.min unexploredEdges

readWrite "data/ch3/dataset_200_7.txt" strandsD (solveDbg)



