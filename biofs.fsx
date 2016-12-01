#I __SOURCE_DIRECTORY__

// #I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#r "packages/NUnit/lib/net40/nunit.framework.dll"
#r "packages/FSharpx.Collections/lib/net40/FSharpx.Collections.dll"
#load "./Parse.fs"
open Bio.Parse
open Bio.Type
open Bio.Utils
// open Bio.Euler
#load "./Ba3.fs"
open FParsec
open System.IO

open FSharpx.Collections.LazyList

open Bio.deBruijn
open Bio.Euler
open Bio.EulerPath
open Bio.kUniversal

open FSharpx.Collections
open FSharpx.Collections.LazyList

open Bio.GapPatterns
// Generate the (3,2)-mer composition of TAATGCCATGGGATGTT in
// lexicographic order. Include repeats, and return your
// answer as a list on a single line.
let s = "TAATGCCATGGGATGTT"
let ls = LazyList.ofSeq s

// StringSpelledByGappedPatterns

// 1. parse
let inp = """4 2
GACC|GCGC
ACCG|CGCC
CCGA|GCCG
CGAG|CCGG
GAGC|CGGA
"""

let (Success(((k, d), dnas), _, _)) = run (gappedPatP) inp


let l2tup = function
    | (x :: [y]) -> (x, y)
    | l -> failwithf "Need list of 2 elements. Input had %d" (List.length l)

let pDNA1 = many1 nucleotide
let barSepStr = (sepBy1 pDNA1 (pchar '|')) |>> l2tup
let barSepStrs = (sepEndBy barSepStr nl)
let gappedPatP = intWs .>>. intWs .>>. barSepStrs
let (Success(((k, d), dnas), _, _)) = run (gappedPatP) inp

dnas

// 2. Stitch sequences

let fsts = List.map fst dnas
let snds = List.map snd dnas



let Ba3jMainf fn = readWrite fn gappedPatP solveGappedPatterns
Ba3jMainf "data/ch3/dataset_6206_7.txt"

solveGappedPatterns
stitchZip fsts snds 2

let xs, ys = stitchZip fsts snds 2



// let filePath = "data/ch3/rosalind_ba3j.txt"
// let sq = System.IO.File.ReadLines(filePath)
// let inp2 = String.concat "\n" sq
// let (Success(((k, d), dnas), _, _)) = run (gappedPatP) inp2
// solveGappedPatterns ((k, d), dnas)

eqWhile (List.skip 3 xs) ys

List.take 6 xs
List.skip 6 xs

ch2str xs
ch2str ys
xs
ys
String.concat "" xs
stitch fsts


kdmerStr s 3 2 |> showKdmers |> printf "%A"

kdmer s 3 1
kdmerStr s 3 1
Seq.chunkBySize 3 s

#time
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
