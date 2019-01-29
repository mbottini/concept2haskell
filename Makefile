all:
	ghc -dynamic test

clean:
	rm -f *.o
	rm -f DataTypes/*.o
	rm -f *.hi
	rm -f DataTypes/*.hi
	rm -f test
