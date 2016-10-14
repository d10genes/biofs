#I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#r "packages/NUnit/lib/net40/nunit.framework.dll"
#load "./Parse.fs"
open Bio.Parse
open Bio.Type
open Bio.Utils
// open Bio.deBruijn
open Bio.Euler
open Bio.DegreeMeasure
#load "./Ba3.fs"
open Ba3.Ba3f
open FParsec
open System.IO

open Ba3.Ba3g

Ba3gMain ()

// Ba3gMain ()

let inp = """0 -> 2
1 -> 3
2 -> 1
3 -> 0,4
6 -> 3,7
7 -> 8
8 -> 9
9 -> 6
"""

let (Success((em), _, _)) = run (pEulCycMap) inp
let (Success((tl), _, _)) = run (pEulCyc) inp

// let em = tupsL2eM tl
// solveEulerPath em

// run pEulCycLine "14 -> 3"  // Success: (14, [3])
// run pEulCycLine "14 -> 30,23"  // Success: (14, [30; 23])
// run pEulCyc inp

tl


let em', xEdge = tl |> toCycle
let path, _ = eulerCycle em'
toCycle tl
path







eulerPath tl
path
unsplicePath path xEdge

m_ |> Map.map (keyF ( (<>) 0))
