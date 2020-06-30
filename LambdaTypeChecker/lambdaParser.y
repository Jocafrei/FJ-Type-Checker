{
module Main where
import Data.Char
import LambdaTypeChecker
}
%name parser
%tokentype { Token      }
%error     { parseError }
%token
	"true"     { TokenTrue   }
	"false"    { TokenFalse  }
	'\\'       { TokenLambda }
	'('        { TokenOB     }
	')'        { TokenCB     }
	"->"       { TokenArrow  }
	' '        { TokenSpace  }
	var        { TokenVar $$ }
%%
Term : "true"              { TTrue      }
     | "false"             { TFalse     }
     | var                 { TVar $1    }
     | '\\' var "->" Term  { TAbs $2 $4 }
     | Term ' ' Term       { TApp $1 $3 }
     | '(' Term ')'        { $2         }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
		= TokenTrue
		| TokenFalse
		| TokenVar String
		| TokenLambda
		| TokenOB
		| TokenCB
		| TokenSpace
		| TokenArrow
		deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
	  | isAlpha c = lexVar (c:cs)
lexer('\\'    :cs)   = TokenLambda : lexer cs
lexer('('     :cs)   = TokenOB     : lexer cs
lexer(')'     :cs)   = TokenCB     : lexer cs
lexer('-':'>' :cs)   = TokenArrow  : lexer cs
lexer(' '     :cs)   = TokenSpace  : lexer cs
lexer(_       :cs)   = lexer cs
lexVar cs = 
  case span isAlpha cs of
    ("true", rest)  -> TokenTrue    : lexer rest
    ("false", rest) -> TokenFalse   : lexer rest
    (var, rest)     -> TokenVar var : lexer rest
main = do getLine >>= print . parser . lexer
          main
}