Interpreter: Tokens.hs Grammar.hs Interpreter.hs
						 ghci Interpreter.hs
Tokens.hs: Tokens.x
					 alex Tokens.x

Grammar.hs: Grammar.y
					  happy Grammar.y
