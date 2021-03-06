all:
	ghc ergparse

clean:
	rm -f *.o
	rm -f DataTypes/*.o
	rm -f *.hi
	rm -f DataTypes/*.hi
	rm -f ergparse
	rm -f *.pdf
	rm -f *.html
	rm -f *.txt

pdf:
	pandoc -V geometry:margin=1in -o writeup.pdf writeup.md
