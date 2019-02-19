all:
	ghc test

clean:
	rm -f *.o
	rm -f DataTypes/*.o
	rm -f *.hi
	rm -f DataTypes/*.hi
	rm -f test
	rm -f *.pdf
	rm -f *.html
	rm -f *.txt
