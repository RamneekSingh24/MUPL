## Requirements:
- Alex
   ```bash
   cabal install alex
   ```
- bash sed tool 
   ```bash
   sudo apt-get install sed
   ```

## Usage

### BigInteger module
- add:: String -> String -> Int -> String
- mul:: String -> String -> Int -> String
- karatsuba:: [Int] -> [Int] -> Int -> [Int]

Examples:
```bash
ghci BigInteger.hs
*BigInteger> add "12345677888888" "12213331414134553511" 10
"12213343759812442399"

*BigInteger> mul "12345676288934189398" "1328343815831285" 10
"16399302750660658942718531003716430"

*BigInteger> karatsuba [1,2,3,4,4,5,5,6,6,6,8,9,1,2,3,4,5] [2,3,4,5,6,6,7,7,7,8,8,8,1,9] 10
[2,7,6,1,1,6,5,0,9,1,1,8,6,2,9,3,6,7,4,5,0,5,5,9,0,8,5,1,9,9,4,0,0,0]

karatsuba (fromString "12344555555555") (fromString "12343455561234142") 10
[0,1,8,8,5,7,3,4,2,8,1,1,8,8,1,9,7,1,3,2,9,2,7,4,4,7,3,2,5,1,0,0,0,0]

*BigInteger> toString (karatsuba (fromString "123434141212") (fromString "134133431") 10)
"16556644863304058372"
```


### Parser module
- See ebnf.txt for grammer of language.
- parse:: String -> Expr, Takes in a program as input and returns the AST. 
- Parsing a file according to assignment specs:
  ```bash
  make
  ./a2 <filename>
  >> Output of lexing and parsing
  make clean
  ```

Examples:

```bash
ghci Parser.hs
*Parser> parse "5 PLUS let x = 5 in x TIMES 10 PLUS NEGATE 3 end"
Bi Plus (Num 5) (LetIn ("x",Num 5) (Bi Plus (Bi Times (Id "x") (Num 10)) (Uni Negate (Num 3))))
```

```bash
make
./a2 prog.txt
[LET "let",ID "x",ASSIGN "=",INT 5,IN "in",LET "let",ID "y",ASSIGN "=",INT 10,IN "in",IF "if",ID "x",PLUS "PLUS",ID "y",EQUALS "EQUALS",INT 15,THEN "then",INT 66,ELSE "else",INT 77,FI "fi",END "end",END "end",EOF "EOF"]
[NodeLetIn,NodeLetExp,NodeVariableBinding,NodeBindingName "x",NodeBindingVal,LeafNum 5,NodeInExp,NodeLetIn,NodeLetExp,NodeVariableBinding,NodeBindingName "y",NodeBindingVal,LeafNum 10,NodeInExp,NodeIfThenElse,NodeIfExp,NodeBinOp Equals,NodeBinOp Plus,LeafVar "x",LeafVar "y",LeafNum 15,NodeThenExp,LeafNum 66,NodeElseExp,LeafNum 77]

make clean
```

Where prog.txt is 
```txt
let 
    x = 5
in
    let 
        y = 10
    in
        if x PLUS y EQUALS 15 
        then 66
        else 77
    fi  
    end
end
EOF
```




