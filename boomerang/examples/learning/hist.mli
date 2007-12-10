(** a histogram for a given token and set of chunks 

    histograms record how many chunks have the token at each frequency
*)
type t

type histogram = Lex.token * t

type histograms = histogram list

(** makes a histogram for each token appearing in the input 

    no searching inside of metatokens is done
*)
val make : (Lex.token list) list -> histograms

(** renders a histogram for a given token as a string
    
    it will be shown as a list of pairs of numbers
    
    the first number in each pair is a frequency: how often does the
    token for that histogram appear

    the second number in each pair is the number of chunks in which
    the given token appears with the frequency

    that is, for a token, t, one sample histogram will be rendered as:

    [(0, 6); (3, 50); (4, 20)]

    This means that in a set of 76 chunks, 6 chunks didn't have the
    token, 50 contained three instances of the token, and 20 had 4
    instances of the token.
*)
val to_string : t -> string

(** the size of a histogram, defined as the number of samples

    for example, the size of the histogram in to_string is 76
*)
val size : t -> int

(** the width of a histogram, defined as the number of different
    frequencies at which chunks have a given token 

    for example, the width of the histogram in to_string is 2: there
    are two frequencies other than 0, namely 3 and 4
*)
val width : t -> int

(** the residual mass of the most common frequency of the token for
    the given histogram

    the residual mass of the sample histogram in to_string is 26:
    there are 26 chunks which don't match the most frequent case
*)
val residual_mass : t -> int

(** the coverage of the histogram, which is the number of chunks in
    which the histogram's token appears at least once

    for the sample histogram in to_string, the coverage is 70
*)
val coverage : t -> int

(** computes the symmetric relative entropy (Jensen-Shannon
    divergence) of two histograms.  this is effectively a similarity
    measure on two histograms
*)
val relative_entropy : t -> t -> float
