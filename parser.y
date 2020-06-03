{
module Main where
import Data.Char
import FJTypeChecker
}

%name parser
%tokentype { Token      }
%error     { parseError }
%token
      "new"        { TokenNew        }
      "class"      { TokenClass      }
      "extends"    { TokenExtends    }
      "super"      { TokenSuper      }
      "boolean"    { TokenBool       }
      "int"        { TokenIntTy      }
      "return"     { TokenReturn     }
      "implements" { TokenImplements }
      "while"      { TokenWhile      }
      '('          { TokenOB         }
      ')'          { TokenCB         }
      '{'          { TokenOCB        }
      '}'          { TokenCCB        }
      '.'          { TokenDot        }
      ','          { TokenComma      }
      ';'          { TokenSemi       }
      '='          { TokenEquals     }
      '+'          { TokenPlus       }
      '-'          { TokenMinus      }
      '/'          { TokenDiv        }
      '*'          { TokenMult       }
      "=="         { TokenEq         }
      "!="         { TokenNeq        }
      '\\'         { TokenLambda     }
      "->"         { TokenArrow      }
      var          { TokenVar $$     }
      int          { TokenInt $$     }
%%

ClassDeclaration : "class" var "extends" var "implements" var '{' ListTyString Constructor Methods '}' { Class $2 $4 $6 $8 $9 $10 }

Term : var                                               { TVar $1                    }
     | Term '.' var                                      { TFieldAcc $1 $3            }
     | "new" var '(' Terms ')'                           { TObjCre $2 $4              }
     | '(' var ')' Term                                  { TCast $2 $4                }
     | List                                              { TFields $1                 }
     | Term '.' var '(' Terms ')'                        { TMethodInv $1 $3 $5        }
     | Value                                             { TValue $1                  }

Operator : '+'                                           {                            }
         | '-'                                           {                            }
         | '*'                                           {                            }
         | '/'                                           {                            }

Operation : var                                          { OpString $1                }
          | int                                          { OpInt $1                   }
          | Operation Operator Operation                 { Oper $1 $3                 }

Command : "while" '(' Expression ')' '{' CommandList '}' { ComWhile $3 $6             }
        | var '=' Operation ';'                          { ComAssign $1 $3            }
        | Ty var '=' Operation ';'                       { ComDeclare $1 $2 $4        }

CommandList :                                            { []                         }
            | Command CommandList                        { $1 : $2                    }

Comparator : "=="                                        {                            }
           | "!="                                        {                            }

Expression : var Comparator int                          { Expr $1 $3                 }

Value : '\\' Parameters "->" Term                        { PureLambdaExpression $2 $4 }

Parameters : Parameter                                   { [$1]                       }
           | Parameters ',' Parameter                    { $3 : $1                    }

Parameter : var                                          { Untyped $1                 }
          | Ty var                                       { Typed $1 $2                }

Constructor : var '(' Arguments ')' '{' "super" '(' Strings ')' ';' ThisList '}' { Cons $1 $3 $8 $11 }

Method : Header '{' CommandList "return" Term ';' '}'    { Met $1 $3 $5               }

Header : Ty var '(' Arguments ')'                        { Head $1 $2 $4              }

Methods : Method                                         { [$1]                       }
        | Methods Method                                 { $2 : $1                    }

TyString : Ty var ';'                                    { ($1, $2)                   }

ListTyString :                                           { []                         }
             | ListTyString TyString                     { $2 : $1                    }

Arguments :                                              { []                         }
          | Ty var                                       { [($1,$2)]                  }
          | Ty var ',' Arguments                         { (($1,$2) : $4)             }

Strings :                                                { []                         }
        | var                                            { [$1]                       }
        | Strings ',' var                                { $3 : $1                    }

ThisList : var '.' var '=' var ';'                       { if $1 == "this" then [($3,$5)] else error "this identifier expected"      }
         | var '.' var '=' var ';' ThisList              { if $1 == "this" then (($3,$5) : $7) else error "this identifier expected" }

Ty : "boolean"                                           { TyBool                     }
   | "int"                                               { TyInt                      }
   | var                                                 { TyClass $1                 }

List : var var ';'                                       { [($1,$2)]                  }
     | var var ';' List                                  { (($1,$2) : $4)             }

Terms : Term                                             { [$1]                       }
      | Terms ',' Term                                   { $3 : $1                    }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenEq
      | TokenOB
      | TokenCB
      | TokenNew
      | TokenDot
      | TokenOCB
      | TokenCCB
      | TokenNeq
      | TokenDiv
      | TokenPlus
      | TokenMult
      | TokenSemi
      | TokenBool
      | TokenMinus
      | TokenClass
      | TokenComma
      | TokenSuper
      | TokenArrow
      | TokenWhile
      | TokenIntTy
      | TokenEquals
      | TokenReturn
      | TokenLambda
      | TokenInt Int
      | TokenExtends
      | TokenImplements
      | TokenVar String
      deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
	  | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('('    :cs)  = TokenOB     : lexer cs
lexer (')'    :cs)  = TokenCB     : lexer cs
lexer ('.'    :cs)  = TokenDot    : lexer cs
lexer (','    :cs)  = TokenComma  : lexer cs
lexer (';'    :cs)  = TokenSemi   : lexer cs
lexer ('{'    :cs)  = TokenOCB    : lexer cs
lexer ('}'    :cs)  = TokenCCB    : lexer cs
lexer ('=':'=':cs)  = TokenEq     : lexer cs
lexer ('!':'=':cs)  = TokenNeq    : lexer cs
lexer ('='    :cs)  = TokenEquals : lexer cs
lexer ('+'    :cs)  = TokenPlus   : lexer cs
lexer ('/'    :cs)  = TokenDiv    : lexer cs
lexer ('*'    :cs)  = TokenMult   : lexer cs
lexer ('\\'   :cs)  = TokenLambda : lexer cs
lexer ('-':'>':cs)  = TokenArrow  : lexer cs
lexer ('-'    :cs)  = TokenMinus  : lexer cs
lexer (_      :cs)  = lexer cs
lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs
lexVar cs = 
  case span isAlpha cs of
    ("new", rest)        -> TokenNew        : lexer rest
    ("class", rest)      -> TokenClass      : lexer rest
    ("extends", rest)    -> TokenExtends    : lexer rest
    ("super", rest)      -> TokenSuper      : lexer rest
    ("boolean", rest)    -> TokenBool       : lexer rest
    ("return", rest)     -> TokenReturn     : lexer rest
    ("implements", rest) -> TokenImplements : lexer rest
    ("while", rest)      -> TokenWhile      : lexer rest
    ("int", rest)        -> TokenIntTy      : lexer rest
    (var, rest)          -> TokenVar var    : lexer rest
main = do getContents >>= print . buildTables . parser . lexer
          main          
}