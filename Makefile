SOURCES = Makefile $(wildcard **/*.ml) $(wildcard **/*.mli) $(wildcard **/*.mly) $(wildcard **/*.mll)
BINS = novelty_stats find_novelties sort_pgn


all: $(BINS)

novelty_stats: _build/default/src/novelty_stats.exe
	cp -f _build/default/src/novelty_stats.exe novelty_stats

_build/default/src/novelty_stats.exe : $(SOURCES)
	dune build src/novelty_stats.exe

find_novelties: _build/default/src/find_novelties.exe
	cp -f _build/default/src/find_novelties.exe find_novelties

_build/default/src/find_novelties.exe : $(SOURCES)
	dune build src/find_novelties.exe

sort_pgn: _build/default/src/sort_pgn.exe
	cp -f _build/default/src/sort_pgn.exe sort_pgn

_build/default/src/sort_pgn.exe : $(SOURCES)
	dune build src/sort_pgn.exe


clean:
	dune clean
	rm -f $(BINS)

distclean: clean
	rm -f pgn_parse.ml pgn_parse.mli pgn_lex.ml pgn_lex.mli
