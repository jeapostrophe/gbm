module GBM.AST where

import qualified Data.ByteString as B
import Data.Int
import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc
import Data.Word

--- Common

type Var = String

type Field = Var

type FunName = String

data SrcLoc = SrcLoc

data RType
  = T_U1
  | T_U8
  | T_S8
  | T_U16
  | T_S16
  | T_U32
  | T_S32
  | T_F32
  | T_BS Word16

--- The compile-time language
{- A compile-time program is evaluated into a run-time program. -}

--- Heap format
{- A heap format describes the shape of memory manipulated by the program -}

data HValue
  = HU1 SrcLoc Bool
  | HU8 SrcLoc Word8
  | HS8 SrcLoc Int8
  | HU16 SrcLoc Word16
  | HS16 SrcLoc Int16
  | HU32 SrcLoc Word32
  | HS32 SrcLoc Int32
  | HF32 SrcLoc Float
  | HBS SrcLoc B.ByteString
  | HVUnion SrcLoc Field HValue
  | HVStruct SrcLoc [(Field, HValue)]
  | HVArray SrcLoc [HValue]

data HFormat
  = HVal SrcLoc RType
  | HUnion SrcLoc (M.Map Field HFormat)
  | HStruct SrcLoc [(Field, HFormat)]
  | HArray SrcLoc HFormat Word16

data Heap
  = Heap HFormat HValue

--- The run-time language
{- A run-time program is a read-only heap and a set of functions that
 receive a read/write heap and an argument heap -}

data RPrim
  = NEG
  | ADD
  | SUB
  | MUL
  | DIV
  | REM
  | SHL
  | SHR
  | BAND
  | BIOR
  | BXOR
  | ABS
  | FABS
  | FPOW
  | FSIN
  | FCOS
  | FEXP
  | FLOG
  | FSQRT
  | CLT
  | CLE
  | CEQ
  | CNE
  | CGE
  | CGT
  | FFLR
  | FCEI
  | FROU
  | FTRU

data HPathComponent
  = HVariant SrcLoc Field
  | HField SrcLoc Field
  | HElem SrcLoc RArg

data HRoot
  = ROM
  | RAM
  | ARG
  | HObj RArg

data HPath
  = HPath HRoot [HPathComponent]

data RArg
  = RRef SrcLoc Var
  | RVal SrcLoc HValue
  | RHeap SrcLoc HPath

data RExpr
  = RPrimApp SrcLoc RPrim [RArg]
  | RCast SrcLoc RArg RType
  | REArg SrcLoc RArg

data RSetLHS
  = RSHeap SrcLoc HPath
  | RSVar SrcLoc Var

data RStmt
  = RSet SrcLoc RSetLHS RArg
  | RLet SrcLoc Bool Var HFormat RExpr RStmt
  | RSeqn SrcLoc RStmt RStmt
  | RIf SrcLoc RArg RStmt RStmt
  | RWhile SrcLoc RArg RStmt
  | RPrompt SrcLoc Var RStmt
  | RRepeat SrcLoc Var
  | REscape SrcLoc Var
  | RTrace SrcLoc [RArg]
  | RVoid SrcLoc

data RFun
  = RFun SrcLoc HFormat RStmt

data RProg
  = RProg Heap HFormat (M.Map FunName RFun)

---

cBraces :: Doc a -> Doc a
cBraces body = lbrace <> (nest 2 $ hardline <> body) <> hardline <> rbrace

cIf :: Doc a -> Doc a -> Doc a -> Doc a
cIf c t f = "if" <+> parens c <+> cBraces t <+> "else" <+> cBraces f

cWhile :: Doc a -> Doc a -> Doc a
cWhile c b = "while" <+> parens c <+> cBraces b

class AsC a where
  as_c :: a -> Doc ann

instance AsC String where
  as_c = pretty

instance AsC RType where
  as_c = \case
    T_U1 -> "bool"
    T_U8 -> u 8
    T_S8 -> s 8
    T_U16 -> u 16
    T_S16 -> s 16
    T_U32 -> u 32
    T_S32 -> s 32
    T_F32 -> "float"
    T_BS len -> "char" <> brackets (pretty len)
    where u n = "u" <> s n
          s (n::Int) = "int" <> pretty n <> "_t"

instance AsC HValue where
  as_c = \case
    HU1 _ True -> "true"
    HU1 _ False -> "false"
    HU8 _ n -> num T_U8 n
    HS8 _ n -> num T_S8 n
    HU16 _ n -> num T_U16 n
    HS16 _ n -> num T_S16 n
    HU32 _ n -> num T_U32 n
    HS32 _ n -> num T_S32 n
    HF32 _ n -> num T_F32 n
    HBS _ b -> dquotes $ pretty $ B.unpack b
    HVUnion _ f hv -> cFieldVals $ [(f, hv)]
    HVStruct _ fs -> cFieldVals fs
    HVArray _ vs -> braces $ hsep $ punctuate comma $ map as_c vs
    where cFieldVals = cBraces . vsep . map cFieldVal
          cFieldVal (f, hv) = "." <> pretty f <+> "=" <+> as_c hv <> comma
          num t n = parens (as_c t) <+> pretty n

instance AsC HFormat where
  as_c = \case
    HVal _ t -> as_c t
    HUnion _ m -> "union" <+> cBraces (cFields $ M.toList m)
    HStruct _ fs -> "struct" <+> cBraces (cFields fs)
    HArray _ t sz -> as_c t <> brackets (pretty sz)
    where cField (f, t) = as_c t <+> pretty f <> semi
          cFields = vsep . map cField

instance AsC HPathComponent where
  as_c = \case
    HVariant _ f -> "." <> pretty f
    HField _ f -> "." <> pretty f
    HElem _ a -> brackets $ as_c a

instance AsC HRoot where
  as_c = \case
    ROM -> "rom"
    RAM -> "ram"
    ARG -> "*arg"
    HObj a -> as_c a

instance AsC HPath where
  as_c (HPath r pcs) = parens (as_c r) <> (hcat $ map as_c pcs)

instance AsC RArg where
  as_c = \case
    RRef _ v -> pretty v
    RVal _ v -> as_c v
    RHeap _ h -> as_c h

as_cp :: RPrim -> [Doc a] -> Doc a
as_cp = \case
  NEG -> una "-"
  ADD -> bin "+"
  SUB -> bin "-"
  MUL -> bin "*"
  DIV -> bin "/"
  REM -> bin "%"
  SHL -> bin "<<"
  SHR -> bin ">>"
  BAND -> bin "&"
  BIOR -> bin "|"
  BXOR -> bin "^"
  ABS -> fun "abs"
  FABS -> fun "fabs"
  FPOW -> fun "powf"
  FSIN -> fun "sinf"
  FCOS -> fun "cosf"
  FEXP -> fun "expf"
  FLOG -> fun "logf"
  FSQRT -> fun "sqrtf"
  CLT -> bin "<"
  CLE -> bin "<="
  CEQ -> bin "=="
  CNE -> bin "!="
  CGE -> bin ">="
  CGT -> bin ">"
  FFLR -> fun "floorf"
  FCEI -> fun "ceilf"
  FROU -> fun "roundf"
  FTRU -> fun "truncf"
  where una op [ x ] = op <+> x
        una _ _ = error "impossible"
        bin op [ x, y ] = x <+> op <+> y
        bin _ _ = error "impossible"
        fun f as = f <> parens ( hsep $ punctuate comma as )

instance AsC RExpr where
  as_c = \case
    RCast _ a t -> parens (as_c t) <> parens (as_c a)
    RPrimApp _ p as -> as_cp p $ map as_c as
    REArg _ a -> as_c a

instance AsC RSetLHS where
  as_c = \case
    RSHeap _ h -> as_c h
    RSVar _ v -> as_c v

instance AsC RStmt where
  as_c = \case
    RSet _ hp a -> as_c hp <+> "=" <+> as_c a <> semi
    RLet _ isConst v hf re k ->
      (if isConst then "const " else "") <> as_c hf <+> as_c v <+> "=" <+> as_c re <> semi <> hardline <> as_c k
    RSeqn _ x y -> as_c x <> hardline <> as_c y
    RIf _ c t f -> cIf (as_c c) (as_c t) (as_c f)
    RWhile _ c s -> cWhile (as_c c) (as_c s)
    RPrompt _ lab s ->
      pretty (before lab) <> ":" <+> cBraces (as_c s) <> hardline <> pretty (after lab) <> ":" <+> semi
    RRepeat _ lab -> "goto" <+> pretty (before lab) <> semi
    REscape _ lab -> "goto" <+> pretty (after lab) <> semi
    RTrace _ _args -> "printf" <> parens (dquotes $ "XXX trace\n")
    RVoid _ -> emptyDoc
    where after = ("after_" ++)
          before = ("before_" ++)

instance AsC RFun where
  as_c (RFun _ args body) =
    parens (as_c args <+> as_c ARG) <+> cBraces (as_c body)

instance AsC RProg where
  as_c (RProg (Heap romf romv) ramf funs) =
    vsep $
      [ "#include <stdio.h>"
      , "#include <stdbool.h>"
      , "#include <stdint.h>"
      , emptyDoc
      , "static" <+> "const" <+> as_c romf <+> as_c ROM <+> "=" <+> as_c romv <> semi
      , "static" <+> as_c ramf <+> as_c RAM <> semi
      , emptyDoc
      ]
        ++ funs'
    where
      funs' = map (\(fn, rf) -> "void" <+> pretty fn <+> as_c rf) $ M.toList funs

---

mt :: SrcLoc
mt = SrcLoc

hnull :: HFormat
hnull = (HStruct mt mempty)

fib_iter :: RStmt
fib_iter =
  let ansp = HPath ARG [ HField mt "ans" ]
      np = HPath ARG [ HField mt "n" ]
      zero = RVal mt (HU32 mt 0)
      one = RVal mt (HU32 mt 1)
  in
    RLet mt False "n" (HVal mt T_U32) (REArg mt (RHeap mt np)) $
    RLet mt False "acc" (HVal mt T_U32) (REArg mt zero) $
    RLet mt False "prev" (HVal mt T_U32) (REArg mt one) $
    RPrompt mt "ell" $
    RLet mt False "stop" (HVal mt T_U1) (RPrimApp mt CLT [ (RRef mt "n") , one ]) $
    RIf mt (RRef mt "stop")
    (RSet mt (RSHeap mt ansp) (RRef mt "acc")) $
    RLet mt True "nm1" (HVal mt T_U32) (RPrimApp mt SUB [ (RRef mt "n"), one ]) $
    RLet mt True "ppa" (HVal mt T_U32) (RPrimApp mt ADD [ (RRef mt "prev"), (RRef mt "acc") ]) $
    RSeqn mt (RSet mt (RSVar mt "n") (RRef mt "nm1")) $
    RSeqn mt (RSet mt (RSVar mt "prev") (RRef mt "acc")) $
    RSeqn mt (RSet mt (RSVar mt "acc") (RRef mt "ppa")) $
    RRepeat mt "ell"

ex_rp :: RProg
ex_rp = RProg
        (Heap hnull (HVStruct mt mempty))
        hnull
        $ M.fromList
        [ ("test"
          , RFun mt (HStruct mt [ ("n", (HVal mt T_U32))
                                , ("ans", (HVal mt T_U32))]) fib_iter )
        ]
  
example :: Doc a
example =
  as_c ex_rp

main :: IO ()
main = do
  putStrLn $ "// Get Bonus Machine"
  putStrLn $ show example
