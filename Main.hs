import Data.Tree
import Text.Parsec
import Control.Applicative (some)
import Control.Monad
import Data.Either
import Data.Function
import Data.Char

src = "(Program\n\
\  (StatementSeq\n\
\    (BlockStatement\n\
\      (SubStatement\n\
\        (sub)\n\
\        (SubNameExpr (foo))\n\
\        (SubDefinition\n\
\          (Block\n\
\            ({)\n\
\            (StatementSeq\n\
\              (Statement\n\
\                (NonBraceExprAssignR\n\
\                  (NonBraceExprValue0\n\
\                    (NonBraceValue\n\
\                      (NonLiteral\n\
\                        (Modifier (my))\n\
\                        (ParenExpr\n\
\                          (()\n\
\                          (ExprValueR\n\
\                            (Value\n\
\                              (NonLiteral\n\
\                                (Variable\n\
\                                  (VarScalar ($) (VarIdentExpr (char)) (ElemSeq0))))))\n\
\                          ())))))\n\
\                  (=)\n\
\                  (ExprValueR\n\
\                    (Value\n\
\                      (NonLiteral\n\
\                        (Variable (GlobalVarExpr (@) (_) (ElemSeq0))))))))\n\
\              (;))\n\
\            (})))))))"

ident = satisfy (\x -> not (isSpace x) && x /= '(' && x /= ')')

parser = Node <$> (char '(' *> many ident) <*> many (spaces *> parser) <* char ')'

toHaskell :: Tree String -> String
toHaskell (Node "SubNameExpr" [x]) = "func " <> rootLabel x
toHaskell (Node a []) = ""
toHaskell (Node a xs) = xs >>= toHaskell

main :: IO ()
main = case toHaskell <$> parse parser "" src of
    Left error -> print error
    Right res -> putStrLn res
