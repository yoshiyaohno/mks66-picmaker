picmaker: picmaker.hs
	ghc -dynamic picmaker

run: picmaker
	./picmaker > pic.ppm

clean:
	rm picmaker pic.ppm *.hi *.o
