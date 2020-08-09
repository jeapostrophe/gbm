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

data HFormat
  = HVal SrcLoc RType
  | HUnion SrcLoc (M.Map Field HFormat)
  | HStruct SrcLoc [(Field, HFormat)]

data Heap
  = Heap HFormat HValue

--- The run-time language
{- A run-time program is a read-only heap and a set of functions that
 receive a read/write heap and an argument heap -}

data HPathComponent
  = HField SrcLoc Field
  | HVariant SrcLoc Field
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

data RExpr
  = RPrimApp SrcLoc RPrim [RArg]
  | RHeapRef SrcLoc HPath
  | RCast SrcLoc RArg RType

data RStmt
  = RSet SrcLoc HPath RArg
  | RLet SrcLoc Var HFormat RExpr RStmt
  | RSeqn SrcLoc RStmt RStmt
  | RIf SrcLoc RArg RStmt RStmt
  | RWhile SrcLoc RArg RStmt
  | RPrompt SrcLoc Var RStmt
  | REscape SrcLoc Var
  | RTrace SrcLoc [RArg]

data RFun
  = RFun SrcLoc HFormat RStmt

data RProg
  = RProg Heap HFormat (M.Map FunName RFun)

---

cBraces :: Doc a -> Doc a
cBraces body = braces (nest 2 $ hardline <> body <> space)

cIf :: Doc a -> Doc a -> Doc a -> Doc a
cIf c t f = "if" <+> parens c <+> cBraces t <> hardline <> "else" <+> cBraces f

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
    where cFieldVals = cBraces . vsep . map cFieldVal
          cFieldVal (f, hv) = "." <> pretty f <+> "=" <+> as_c hv
          num t n = parens ( parens (as_c t) <+> pretty n )

instance AsC HFormat where
  as_c = \case
    HVal _ t -> as_c t
    HUnion _ m -> "union" <+> cBraces (cFields $ M.toList m)
    HStruct _ fs -> "struct" <+> cBraces (cFields fs)
    where cField (f, t) = as_c t <+> pretty f <> semi
          cFields = vsep . map cField

instance AsC HPathComponent where
  as_c = \case
    HField _ f -> "." <> pretty f
    HVariant _ f -> "." <> pretty f
    HElem _ a -> brackets $ as_c a

instance AsC HRoot where
  as_c = \case
    ROM -> "rom"
    RAM -> "ram"
    ARG -> "arg"
    HObj a -> as_c a

instance AsC HPath where
  as_c (HPath r pcs) = as_c r <> (hcat $ map as_c pcs)

instance AsC RArg where
  as_c = \case
    RRef _ v -> pretty v

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
    RHeapRef _ h -> as_c h
    RPrimApp _ p as -> as_cp p $ map as_c as

instance AsC RStmt where
  as_c = \case
    RSet _ hp a -> as_c hp <+> "=" <+> as_c a <> semi
    RLet _ v hf re k ->
      as_c hf <+> as_c v <+> "=" <+> as_c re <> semi <> hardline <> as_c k
    RSeqn _ x y -> as_c x <> hardline <> as_c y
    RIf _ c t f -> cIf (as_c c) (as_c t) (as_c f)
    RWhile _ c s -> cWhile (as_c c) (as_c s)
    RPrompt _ lab s -> as_c s <> hardline <> pretty lab <> ":"
    REscape _ lab -> "goto" <+> pretty lab
    RTrace _ _args -> "printf" <> parens (dquotes $ "XXX trace\n")

instance AsC RFun where
  as_c (RFun _ args body) =
    parens (as_c args) <+> cBraces (as_c body)

std_header :: Doc a
std_header = vsep [ "#include <stdio.h>"
                  , "#include <stdbool.h>"
                  , "#include <stdint.h>" ]

instance AsC RProg where
  as_c (RProg (Heap romf romv) ramf funs) =
    vsep $
      [ std_header
      , emptyDoc
      , as_c romf <+> as_c ROM <+> "=" <+> as_c romv <> semi
      , as_c ramf <+> as_c RAM <> semi
      , emptyDoc
      ]
        ++ funs'
    where
      funs' = map (\(fn, rf) -> "void" <+> pretty fn <+> as_c rf) $ M.toList funs

---

ex_rp :: RProg
ex_rp = RProg (Heap (HStruct SrcLoc mempty) (HVStruct SrcLoc mempty)) (HStruct SrcLoc mempty) mempty

example :: Doc a
example =
  as_c ex_rp
