all:
	alex lexer.x -o MyLexer.hs
	sed -i -e 's#AlexError ((AlexPn _ line column),_,_,_) -> error $$ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)#AlexError ((AlexPn _ line column),_,_,tok) -> error $$ "Unkown Token:" ++ (show line) ++ ":" ++ (show column) ++ ":" ++ (show tok)#g' MyLexer.hs
	rm -rf *-e
	ghc main.hs -o a2

clean:
	rm -rf *.o *.hi *-e MyLexer.hs a2 