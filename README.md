# The Agadmator Index: Measuring Novelties in Chess

This repository contains hacky software I wrote to answer the question: What
chess game has the most novelties?

## Disclaimer

I wrote this software to answer a question I had.  It was meant to be run once.
It's neither efficient nor well-written.  I intentionally stuck to high-level
data structures and functional programming idioms because I was curious how the
performance would be.  The answer: not good, but good enough to get the answer.
Which maybe is surprising, considering it builds a transposition table of every
position encountered in the history of recorded chess.

## Building

This is all written in OCaml.  Start by installing the [opam package
manager](https://ocaml.org/docs/install.html#OPAM) if you don't have it.

This was tested on OCaml 4.11.2.  Make yourself a fresh switch (a switch is a
fresh environment with its own copy of the compiler and libraries):

```
$ opam switch create agadmator-index 4.11.2+flambda
```

This will take a minute.  Then install dependencies:

```
$ opam install core core_kernel dune menhir ppx_deriving
```

I probably forgot some of the dependencies.  If I did, when you build you'll get
a helpful error telling you what you're missing.

Finally, build the software:

```
$ make
```

You should see one warning, from the generated parser, because Menhir hasn't
been updated in a while.

## Running

First: you need a database of chess games as a pgn file.  The PGN standard is
not great and PGN files in the wild don't comply to it, so I'm not sure how
flexible this software is, but it's known to work on the utf-8 encoded PGN files
generated by Scid vs PC.  The Scid vs PC database I used is a lightly edited
version of [Caissabase 2021-12-05](http://caissabase.co.uk/).  The version with
my edits is on the release page.  Details of the edits are in
[db-notes.md](db-notes.md).

Building should result in three binaries in the top-level directory: `sort_pgn`,
`find_novelties`, and `novelty_stats`.  These must be run on your database, in
order, to:

1) Sort the games by date.
2) Find the novelties.
3) Compute and output stats about the novelties.

They all take while, but do print status updates.  The usage is:

```
$ sort_pgn <foo.pgn>
```

This will result in a file `foo.pgn.sorted` that has the same games but sorted
into the order we will consider the games to have happened for determening what
counts as a novelty.  This needs about 2.5x the size of the file in RAM, and
takes ~20 minutes for Caissabase on my computer.

```
$ find_novelties <foo.pgn.sorted> <upper_bound_on_games>
```

The first argument is the output of `sort_pgn`.  The second is an integer that
is as least as big as the number of games in the database.  For Caissabase I
just use 5000000.  It will output a file `foo.pgn.sorted.novelties` that is a
serialized OCaml datastructure with information about each games novelties.

This is the most computationally intensive step, as it needs to build the giant
transposition table.  On my computer, for Caissabase, it takes ~46GB of RAM and
45 minutes.

Finally, you can print the stats with:

```
$ find_novelties <foo.pgn.sorted> <foo.pgn.sorted.novelties>
```

This is the quickest and least computationally intensive step (but still may
take few minutes as it needs to parse all the games again).  It is going to dump
a bunch of info to stdout, so you may want to redirect it to a file and monitor
progress with `tail -f`.
