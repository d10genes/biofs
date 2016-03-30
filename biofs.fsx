#I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#load "./Parse.fs"
open Bio.Parse
open Bio.Type
open Bio.Utils
#load "./Ba3.fs"
open Ba3.Ba3c
open FParsec
open System.IO

// let nl = pchar '\n'
let inp = """ATGCG
GCATG
CATGC
AGGCA
GGCAT
"""
let (Success((dnas), _, _)) = run strandsD inp

readWrite "data/ch3/rosalind_ba3c.txt" strandsD mkAlst

