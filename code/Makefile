all: mil funlang oolang
docs:
	make action="haddock --executables --hyperlink-source"
mil:
	cd mil; cabal $(action)
funlang:
	cd funlang; cabal $(action)
oolang:
	cd oolang; cabal $(action)
clean:
	cd mil; cabal clean
	cd funlang; cabal clean
	cd oolang; cabal clean
.PHONY: all mil funlang oolang clean
