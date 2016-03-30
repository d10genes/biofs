#I "./packages/FParsec/lib/net40-client/"
#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "./packages/FParsec/lib/net40-client/FParsec.dll"
#load "./Parse.fs"
open Bio.Parse
open Bio.Type
open Bio.Utils
#load "./Ba3.fs"
open Ba3.Ba3b
open FParsec
open System.IO

// let nl = pchar '\n'
let inp = """ATGCG
GCATG
CATGC
AGGCA
GGCAT
"""

let pref = init
let suff = List.tail

run (strandsD) inp

    

run (strandsD) inp
let (Success((dnas), _, _)) = run strandsD inp

dnas |> recon |> dl2str

readWrite "data/ch3/path.txt" ppath (recon >> dl2str)
readWrite "data/ch3/rosalind_ba3b.txt" ppath (recon >> dl2str)
readWrite "data/ch3/dataset_198_3.txt" ppath (recon >> dl2str)



// let a = "ab\nab\n"
// let trynl = '\n' |> pchar |> attempt
// let chars : (CharStream<unit> -> Reply<char>) = noneOf "\n"
// let line = many1 chars
// let lines = sepBy1 line trynl
// // let lines = sepBy1 line (attempt (pchar '\n'))
// // let lines = sepBy1 line (attempt <| pchar <| '\n')
// run  (lines .>> (pchar '\n')) a

let a = "ab\nab"
let chars : (CharStream<unit> -> Reply<char>) = noneOf "\n"
let nl = pchar '\n'
let line = many1 chars .>> (opt nl)
let lines = many1 line
// let lines = sepBy1 line nl
run  (lines) a
run  (line) "ab\n"










#load "./Parse.fs"
open Bio.Parse
let f1 = recon >> dl2str
readWrite "data/ch3/rosalind_ba3b.txt" ppath f1
readWrite "data/ch3/rosalind_ba3b.txt" ppath (recon >> dl2str)

let fin = "data/ch3/string_composition.txt"
appFname fin ".out"
readWrite 
let a = [1;2;3]
let b = [1;2;3]
// a @ b

File.WriteAllText("data/ch3/string_composition_out.txt", res) 
run pcomp s
d2str dna

ba3a_main "data/ch3/string_composition.txt" "data/ch3/string_composition_out.txt"
ba3a_main "data/ch3/rosalind_ba3a.txt" "data/ch3/rosalind_ba3a_out.txt"
ba3a_main "data/ch3/dataset_197_3.txt" "data/ch3/dataset_197_3_out.txt"