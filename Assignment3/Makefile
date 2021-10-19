all:
	alex lexer.x -o Lexer.hs
	happy grammar.y -o Parser.hs
	ghc Main.hs

clean:
	rm -rf *.hi *.o Main Lexer.hs Parser.hs