# Bio algorithms

Implementations of some bioinformatics algorithms from the Rosalind platform in F#.

Parsers, utility functions and basic implementations can be found in `Parse.fs`. Functions to solve the specific problems can be found in `Ba3.fs` This covers most of the problems in the section Ba3_ of the [textbook track problems](http://rosalind.info/problems/list-view/?location=bioinformatics-textbook-track), for example:

- Construct the De Bruijn Graph of a Collection of k-mers (BA3E)
- Find an Eulerian Path in a Graph (BA3G)
- Reconstruct a String from its k-mer Composition (BA3H)

## To run
I ran most of these interactively after developing them in the `biofs.fsx` script file. After the standard project imports and directives, import the solution function and run it on the input file. To run problem `Ba3j` (*Reconstruct a String from its Paired Composition*), for example, do

	open Ba3.Ba3j
	Ba3jMainf "data/ch3/rosalind_ba3j.txt"