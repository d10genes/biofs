namespace Bio

open FParsec
open System.IO

module Type =
    type Nuke = A | C | G | T
    type DNA = Nuke list
    let readDna = function
        | 'A' -> A
        | 'C' -> C
        | 'G' -> G
        | 'T' -> T
        | _   -> failwith "This isn't a valid nucleotide"

    let showDna = function
        | A -> "A"
        | C -> "C"
        | G -> "G"
        | T -> "T"

    let dl2str dna = dna |> List.map showDna |> String.concat ""
    let ds2str dna = dna |> Seq.map showDna |> String.concat ""

module Parse =
    open Type
    let ws = spaces
    let int = pint32
    let nl = pchar '\n'

    let int_ws : Parser<int32,unit> = pint32 .>> ws

    let nucleotide: (CharStream<unit> -> Reply<char>) = anyOf "ACGT"
    let nucleotideD = nucleotide |>> readDna
    let pDNA = many nucleotide
    let pDNAD = many1 nucleotideD .>> (opt nl)
    let strands = ws >>. (sepBy1 pDNA nl)
    let strandsD = ws >>. many pDNAD // .>> ('\n' |> pchar |> opt)
    // let strandsD = ws >>. (sepBy1 pDNAD nl) // .>> ('\n' |> pchar |> opt)
    let nums: Parser<int32 list,unit> = sepBy1 int (pchar ' ')

    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


module Utils =
    let (&&&) f g x = f x, g x
    let ( *** ) (f: 'a -> 'b) (g: 'c -> 'd) (x, y) = (f x, g y)

    let kmers = Seq.windowed
    let kmerl k xs = Seq.windowed k xs |> Seq.map List.ofArray
    let rec init = function
        | h :: [] -> []
        | h :: tl -> h :: init tl
        | _ -> failwith "Empty list."

    let appFname fn suff =
        let fbase = Path.GetFileNameWithoutExtension fn
        let fext = Path.GetExtension fn
        let fdir = Path.GetDirectoryName fn
        let fn2 = String.concat "" [| fbase; suff; fext |]
        Path.Combine(fdir, fn2)

    let readWrite pathin parser f =
        printfn "Hi"
        let pathout = appFname pathin ".out"
        let textin = File.ReadAllText(pathin)
        match run parser textin with
        | Success(parsed, _, _) ->
            let res = f parsed
            File.WriteAllText(pathout, res)
            printfn "Answer written to %s" pathout
        | Failure _ -> failwith "Invalid parse"

    let rec cartesian = function
        | [] -> Seq.singleton []
        | L::Ls -> cartesian Ls |> Seq.collect (fun c -> L |> Seq.map (fun x->x::c))

