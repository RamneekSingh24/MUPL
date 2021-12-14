## Usage
```bash
make
./main <filename>
make clean
```
## Output Format
Parser output (pre-order trav. of AST)
Type Checker output
Evaluator output

## Example 
```
./main prog.txt
Let (Decl "n" (IntConst 5)) (Let (Decl "f" (LambdaExpr "x" IntType (BinExpr Times (VarExpr "n") (VarExpr "x")) (fromList []))) (Let (Decl "n" (IntConst 8)) (FunAppExpr "f" (VarExpr "n"))))
IntType
IntConst 40
```
where 
prog.txt is 
```
let n:= 5
in
	let f:= fn (x::int) => n*x end
	in
		let n:= 8
		in 
			(f n)
		end
	end
end;
```
