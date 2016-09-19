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

let pEulCycLine = int .>> pstring " -> " .>>. (sepBy int (pchar ','))
// run pEulCycLine "14 -> 3"  // Success: (14, [3])
// run pEulCycLine "14 -> 30,23"  // Success: (14, [30; 23])
let pEulCyc = sepEndBy1 pEulCycLine nl

let tupsL2eL xs =
    let rec tupL2El = function
        | h, (y :: ys) -> (h, y) :: tupL2El (h, ys)
        | h, [] -> []
    List.concat (List.map tupL2El xs)

let (Success((vals), _, _)) = run (pEulCyc |>> tupsL2eL) inp




readWrite "data/ch3/dataset_200_7.txt" strandsD (solveDbg)



