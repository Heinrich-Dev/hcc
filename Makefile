Main:
	ghc --make -dynamic Main.hs
clean:
	rm -f Generator.hi Generator.o
	rm -f Grammar.hi Grammar.o
	rm -f Parser.hi Parser.o
	rm -f Linear.hi Linear.o
	rm -f Main.hi Main.o Main
