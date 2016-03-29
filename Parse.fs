namespace Bio

open FParsec

module Type =
    type Dna = A | C | G | T
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
    
    let int_ws : Parser<int32,unit> = pint32 .>> ws

    let nucleotide: (CharStream<unit> -> Reply<char>) = anyOf "ACGT"
    let nucleotideD = nucleotide |>> readDna
    let pDNA = many nucleotide
    let pDNAD = many nucleotideD
    let strands = ws >>. (sepBy1 pDNA (pchar '\n'))
    let strandsD = ws >>. (sepBy1 pDNAD (pchar '\n'))
    let nums: Parser<int32 list,unit> = sepBy1 int (pchar ' ')
    
    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


module Utils =
    let kmers = Seq.windowed
    