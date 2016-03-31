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


readWrite "data/ch3/dataset_200_7.txt" strandsD (solveDbg)


Ba3e_main ()

let inp = """GAGG
CAGG
GGGG
GGGA
CAGG
AGGG
GGAG
"""

let (Success((vals), _, _)) = run strandsD inp
vals

vals |> grpDBG |> List.map dedupeTup |> showTups

let k, dna = vals


let kms = kmerl k dna
let km = Seq.head kms
let kk = kms |> List.ofSeq |> List.map presuf |> List.sort |> List.groupBy fst





