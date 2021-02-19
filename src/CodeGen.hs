module CodeGen where

import Data.List (intercalate)
import Data.Map qualified as Map
import Expr
import Program

type CGen = String

genFunction :: Name -> Function Type (RTExpr Type) -> CGen
genFunction n (Function args t body) =
  unlines
    [ unwords [genType t, n, wrapParens (intercalate "," (fmap genArg args))],
      genBlock body
    ]
  where
    genArg (n', t') = unwords [genType t', n']

genType :: Type -> String
genType TInt = "int"
genType TDouble = "double"
genType TBool = "int"
genType TVoid = "void"

wrapParens :: String -> String
wrapParens a = "(" <> a <> ")"

wrapBraces :: String -> String
wrapBraces a = "{" <> a <> "}"

genStmt :: Stmt Type (RTExpr Type) -> CGen
genStmt (Return x) = "return " <> genExpr x
genStmt (Decl name t e) =
  unwords [genType t, name, genExpr e]
genStmt (Assign n e) = unwords [n, "=", genExpr e]
genStmt (ExprStmt expr) = genExpr expr

genBlock :: Block Type (RTExpr Type) -> CGen
genBlock block =
  wrapBraces $
    concatMap (\s -> genStmt s <> ";\n") (unBlock block)

genExpr :: RTExpr Type -> CGen
genExpr (RTVar n t) =
  "(" <> genType t <> ")" <> n
genExpr (RTPrim (PInt n) _) =
  show n
genExpr (RTPrim (PDouble d) _) =
  show d
genExpr (RTPrim (PBool b) _) =
  show b
genExpr (RTPrim (PType _) _) =
  error "cannot gen code for type"
genExpr (RTArith op x y _) =
  let cop = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
   in wrapParens (genExpr x) <> cop <> wrapParens (genExpr y)
genExpr (RTBlock block _) = genBlock block
genExpr (RTCall n args _) =
  n <> wrapParens (intercalate "," (fmap genExpr args))

codegen :: RuntimeEnv -> Type -> Block Type (RTExpr Type) -> CGen
codegen (RuntimeEnv fns) _ block =
  unlines $
    fmap (uncurry genFunction) (Map.toList fns)
      <> [genFunction "main" (Function [] TInt block)]
