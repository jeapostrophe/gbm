module GBM.AST where

import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

type Var = String

data GType
  = T_Bool
  | T_Byte
  | T_Short
  | T_Float
  | T_Bytes
  | T_Fun [GType] GType
  | T_Struct (M.Map Var GType)
  | T_Array GType Int
  | T_List GType
  | T_Var Var
  deriving (Eq, Show)

data GPrimitive
  deriving (Eq, Show)

data GExpr
  = GVar Var
  | GBool Bool
  | GByte Int
  | GShort Int
  | GFloat Float
  | GBytes B.ByteString
  | GPrim GPrimitive
  | GBlock [GDefine] GExpr
  | GApp GExpr [GExpr]
  | GCond [(GExpr, GExpr)]
  | GStructRef GExpr Var
  | GStructSet GExpr Var GExpr
  | GArrayRef GExpr GExpr
  | GArraySet GExpr GExpr GExpr
  | GNull
  | GCons GExpr GExpr
  deriving (Eq, Show)

data GDefine
  = GD_Valu Var GExpr
  | GD_Func GType Var [(GType, Var)] GExpr
  | GD_Type Var GType
  | GD_Heap Var GType B.ByteString
  deriving (Eq, Show)

type Bit5 = Int

type GColor = (Bit5, Bit5, Bit5)

type GPal = [GColor]

type Bit16 = Int

type GSpr = [Bit16] --- 8x8

data GCHR = GCHR
            { gchr_pals :: [GPal]
            , gchr_sprs :: [GSpr] }
  deriving (Eq, Show)

data GPRG = GPRG
            { gprg_meta :: GExpr
            , gprg_banks :: [GExpr]
            , gprg_root :: GExpr }
  deriving (Eq, Show)

data GROM
  = GROM_C GCHR GCHR
  | GROM_P GPRG GPRG
  | GROM_B GPRG GCHR
  deriving (Eq, Show)  

example :: GExpr
example =
  GBytes "Test"
