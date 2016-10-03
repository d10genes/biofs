#I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#r "packages/NUnit/lib/net40/nunit.framework.dll"
#load "./Parse.fs"
open Bio.Parse
open Bio.Type
open Bio.Utils
open Bio.deBruijn
open Bio.Euler
#load "./Ba3.fs"
open Ba3.Ba3f
open FParsec
open System.IO

Ba3fMain ()
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


// type UserState = unit // doesn't have to be unit, of course
// type Parser<'t> = Parser<'t, UserState>


// run pEulCycLine "14 -> 3"  // Success: (14, [3])
// run pEulCycLine "14 -> 30,23"  // Success: (14, [30; 23])
// run pEulCyc inp





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



// let ue = Set.ofList vals
// let res2 = remove res (0, 3)

// res2.Item 1 |> Set.toSeq
// // let selectNode =

// let allNodes = unitePairs unexploredEdges
// let startNode = List.ofSeq allNodes |> List.min


// let chooseEdge (edgeCands: EdgeSet<'a>) =
let chooseEdge2 (edgeCands: Set<'a>) =
    if edgeCands.IsEmpty then None
     else Some (Set.toList edgeCands |> List.head)
    // | edgeSet.toSeq |> List.ofSeq



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
let logx x = if (printfn "\t> %A" x) = () then x else x
// let (<!>) x = logx x



let (Success((et), _, _)) = run pEulCycMap inp2
et
/// Insert cycle `subl` into list. Both `subl` and list
/// must begin and end with same element


// splice [1; 5; 1] [0; 3; 2; 4; 2; 1; 0] = [0; 3; 2; 4; 2; 1; 5; 1; 0]




// let p_, e_ = eulerCycle em
let p_, e_ = eulerCycle em



