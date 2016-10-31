#I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#r "packages/NUnit/lib/net40/nunit.framework.dll"
#load "./Parse.fs"
open Bio.Parse
open Bio.Type
open Bio.Utils
// open Bio.Euler
#load "./Ba3.fs"
open FParsec
open System.IO

open Bio.deBruijn
open Bio.EulerPath

// open Ba3.Ba3b
// open Ba3.Ba3g
open Ba3.Ba3h

Ba3hMainf  "data/ch3/StringReconstructionProblem.txt"


let inp = """4
CTTA
ACCA
TACC
GGCT
GCTT
TTAC"""
// let readLines filePath = System.IO.File.ReadAllText(filePath)
// let inp = readLines "data/ch3/StringReconstructionProblem.txt"

// 1. Parse
let reconP = intWs .>>. strandsD
let (Success((k, dnas), _, _)) = run (reconP) inp
// let (Success((res), _, _)) = run (pdbruijn) inp

// 2. Solve
let stringRecon dnas =
    dnas |> grpDBG |> List.map dedupeTup
    |> eulerPath
    |> recon

let r = stringRecon dnas

let solveReconstruction (_, dnas:seq<Nuke list>) =
    stringRecon dnas
    |> List.map showDna |> String.concat ""


let Ba3hMainf fn = readWrite fn reconP solveReconstruction
Ba3hMainf  "data/ch3/StringReconstructionProblem.txt"
Ba3hMainf  "data/ch3/dataset_203_7.txt"
