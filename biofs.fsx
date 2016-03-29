
// #load FParsec
// let pcomp1 = nums .>>. strands
// let pcomp = int_ws .>>. int_ws .>>. strands

#I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#load "./Parse.fs"
#load "./Ba3.fs"
open FParsec
open Bio.Parse
open Bio.Type
open Bio.Utils
open Ba3.Ba3a


let inp = """5
CAATCCAAC"""
let (Success((k, dna), _, _)) = run pcomp inp
dna

let compositionK k dna = 
    k, dna ||> kmers |> Set.ofSeq

let compositionK k = 
    kmers k >> Set.ofSeq

compositionK2 k dna
compositionK2 k dna
    |> Set.map ds2str |> String.concat "\n"  

let compositionKprob k = 
    compositionK k >> Set.map ds2str >> String.concat "\n"  
    // compositionK k dna |> Set.map ds2str |> String.concat "\n"
        
compositionKprob k dna  
compositionK k dna // |> Set.map ds2str |> String.concat "\n"
dna
List.map showDna dna
let pcomp = int_ws .>>. pDNAD
let (Success((k, dna), _, _)) = run pcomp inp



let s = System.IO.File.ReadAllText("data/ch3/string_composition.txt")
let (Success((k, dna), _, _)) = run pcomp s
let res = compositionKshow k dna

System.IO.File.WriteAllText("data/ch3/string_composition_out.txt", res) 
run pcomp s
s
d2str dna

ba3a_main "data/ch3/string_composition.txt" "data/ch3/string_composition_out.txt"
ba3a_main "data/ch3/rosalind_ba3a.txt" "data/ch3/rosalind_ba3a_out.txt"