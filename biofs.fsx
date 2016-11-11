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

// open Ba3.Ba3b
// open Ba3.Ba3g
// open Ba3.Ba3h
open Ba3.Ba3i

Ba3iMainf "data/ch3/universal_string.16.txt"
Ba3iMainf "data/ch3/rosalind_ba3i.txt"

// Profile
open FSharpx.Collections
open FSharpx.Collections.LazyList
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
