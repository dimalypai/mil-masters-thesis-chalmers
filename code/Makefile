all: mil funlang oolang
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
