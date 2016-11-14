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

// Generate the (3,2)-mer composition of TAATGCCATGGGATGTT in
// lexicographic order. Include repeats, and return your
// answer as a list on a single line.
let s = "TAATGCCATGGGATGTT"
let ls = LazyList.ofSeq s
let singleton x = LazyList.cons x LazyList.empty
let kdmer (xs: seq<'a>) (k:int) (d:int) =
    let rec kdmer' (k1: LazyList<'a>) (gap: LazyList<'a>)
        (k2: LazyList<'a>) (rst: LazyList<'a>) (accum)=
        match rst with
        | Nil -> accum
        | Cons(nxt, rst') ->
            let k1' = LazyList.append k1.Tail (singleton gap.Head)
            let gap' = LazyList.append gap.Tail (singleton k2.Head)
            let k2' = LazyList.append k2.Tail (singleton nxt)
            kdmer' k1' gap' k2' rst' (cons (k1', gap', k2') accum)

    let k1, rk1 = Seq.take k xs, Seq.skip k xs
    let g, rg = Seq.take d rk1, Seq.skip d rk1
    let k2, _ = Seq.take k rg, Seq.skip k rg
    kdmer' (LazyList.ofSeq k1) (LazyList.ofSeq g) (LazyList.ofSeq k2)
        (LazyList.ofSeq xs) LazyList.empty
let llToStr (xs: LazyList<char>) = xs |> LazyList.map string |> String.concat ""
let kdmerStr (xs) (k:int) (d:int) =
    kdmer xs k d
        |> LazyList.map (fun (x, _, y) -> llToStr x, llToStr y)
        |> LazyList.toList |> List.sort

let showKdmers xs =
    List.map (fun (x, y) -> String.concat "|" [x; y]) xs
    |> String.concat ","

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
