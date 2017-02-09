{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Casadi.Core.Enums
       (
         Alias(..),
         Category(..),
         Causality(..),
         CollocationPoints(..),
         Dynamics(..),
         Operation(..),
         TypeID(..),
         Variability(..),
       ) where


import Foreign.C.Types ( CInt(..) )
import Casadi.Internal.Marshal ( Marshal(..) )
import Casadi.Internal.WrapReturn ( WrapReturn(..) )

-- EnumDecl: Alias
data Alias = ALIAS
           | NEGATED_ALIAS
           | NO_ALIAS
           deriving (Show, Eq)
instance Enum Alias where
        fromEnum (ALIAS) = 1
        fromEnum (NEGATED_ALIAS) = 2
        fromEnum (NO_ALIAS) = 0
        toEnum (1) = ALIAS
        toEnum (2) = NEGATED_ALIAS
        toEnum (0) = NO_ALIAS
        toEnum k
          = error $ "Alias: toEnum: got unhandled number: " ++ show k
instance Marshal Alias CInt where
        marshal = return . fromIntegral . fromEnum
instance WrapReturn CInt Alias where
        wrapReturn = return . toEnum . fromIntegral

-- EnumDecl: Category
data Category = CAT_ALGEBRAIC
              | CAT_DEPENDENT_CONSTANT
              | CAT_DEPENDENT_PARAMETER
              | CAT_DERIVATIVE
              | CAT_INDEPENDENT_CONSTANT
              | CAT_INDEPENDENT_PARAMETER
              | CAT_STATE
              | CAT_UNKNOWN
              deriving (Show, Eq)
instance Enum Category where
        fromEnum (CAT_ALGEBRAIC) = 7
        fromEnum (CAT_DEPENDENT_CONSTANT) = 3
        fromEnum (CAT_DEPENDENT_PARAMETER) = 5
        fromEnum (CAT_DERIVATIVE) = 1
        fromEnum (CAT_INDEPENDENT_CONSTANT) = 4
        fromEnum (CAT_INDEPENDENT_PARAMETER) = 6
        fromEnum (CAT_STATE) = 2
        fromEnum (CAT_UNKNOWN) = 0
        toEnum (7) = CAT_ALGEBRAIC
        toEnum (3) = CAT_DEPENDENT_CONSTANT
        toEnum (5) = CAT_DEPENDENT_PARAMETER
        toEnum (1) = CAT_DERIVATIVE
        toEnum (4) = CAT_INDEPENDENT_CONSTANT
        toEnum (6) = CAT_INDEPENDENT_PARAMETER
        toEnum (2) = CAT_STATE
        toEnum (0) = CAT_UNKNOWN
        toEnum k
          = error $ "Category: toEnum: got unhandled number: " ++ show k
instance Marshal Category CInt where
        marshal = return . fromIntegral . fromEnum
instance WrapReturn CInt Category where
        wrapReturn = return . toEnum . fromIntegral

-- EnumDecl: Causality
data Causality = INPUT
               | INTERNAL
               | OUTPUT
               deriving (Show, Eq)
instance Enum Causality where
        fromEnum (INPUT) = 0
        fromEnum (INTERNAL) = 2
        fromEnum (OUTPUT) = 1
        toEnum (0) = INPUT
        toEnum (2) = INTERNAL
        toEnum (1) = OUTPUT
        toEnum k
          = error $ "Causality: toEnum: got unhandled number: " ++ show k
instance Marshal Causality CInt where
        marshal = return . fromIntegral . fromEnum
instance WrapReturn CInt Causality where
        wrapReturn = return . toEnum . fromIntegral

-- EnumDecl: CollocationPoints
data CollocationPoints = LEGENDRE
                       | RADAU
                       deriving (Show, Eq)
instance Enum CollocationPoints where
        fromEnum (LEGENDRE) = 0
        fromEnum (RADAU) = 1
        toEnum (0) = LEGENDRE
        toEnum (1) = RADAU
        toEnum k
          = error $ "CollocationPoints: toEnum: got unhandled number: " ++
              show k
instance Marshal CollocationPoints CInt where
        marshal = return . fromIntegral . fromEnum
instance WrapReturn CInt CollocationPoints where
        wrapReturn = return . toEnum . fromIntegral

-- EnumDecl: Dynamics
data Dynamics = ALGEBRAIC
              | DIFFERENTIAL
              deriving (Show, Eq)
instance Enum Dynamics where
        fromEnum (ALGEBRAIC) = 0
        fromEnum (DIFFERENTIAL) = 1
        toEnum (0) = ALGEBRAIC
        toEnum (1) = DIFFERENTIAL
        toEnum k
          = error $ "Dynamics: toEnum: got unhandled number: " ++ show k
instance Marshal Dynamics CInt where
        marshal = return . fromIntegral . fromEnum
instance WrapReturn CInt Dynamics where
        wrapReturn = return . toEnum . fromIntegral

-- EnumDecl: Operation
data Operation = OP_ACOS
               | OP_ACOSH
               | OP_ADD
               | OP_ADDNONZEROS
               | OP_AND
               | OP_ASIN
               | OP_ASINH
               | OP_ASSERTION
               | OP_ASSIGN
               | OP_ATAN
               | OP_ATAN2
               | OP_ATANH
               | OP_BILIN
               | OP_CALL
               | OP_CEIL
               | OP_CONST
               | OP_CONSTPOW
               | OP_COPYSIGN
               | OP_COS
               | OP_COSH
               | OP_DETERMINANT
               | OP_DIAGCAT
               | OP_DIAGSPLIT
               | OP_DIV
               | OP_DOT
               | OP_EQ
               | OP_ERF
               | OP_ERFINV
               | OP_EXP
               | OP_FABS
               | OP_FIND
               | OP_FLOOR
               | OP_FMAX
               | OP_FMIN
               | OP_FMOD
               | OP_GETNONZEROS
               | OP_HORZCAT
               | OP_HORZREPMAT
               | OP_HORZREPSUM
               | OP_HORZSPLIT
               | OP_IF_ELSE_ZERO
               | OP_INPUT
               | OP_INV
               | OP_INVERSE
               | OP_LE
               | OP_LIFT
               | OP_LOG
               | OP_LT
               | OP_MAP
               | OP_MONITOR
               | OP_MTIMES
               | OP_MUL
               | OP_NE
               | OP_NEG
               | OP_NORM1
               | OP_NORM2
               | OP_NORMF
               | OP_NORMINF
               | OP_NOT
               | OP_OR
               | OP_OUTPUT
               | OP_PARAMETER
               | OP_POW
               | OP_PRINTME
               | OP_PROJECT
               | OP_RANK1
               | OP_RESHAPE
               | OP_SETNONZEROS
               | OP_SIGN
               | OP_SIN
               | OP_SINH
               | OP_SOLVE
               | OP_SQ
               | OP_SQRT
               | OP_SUB
               | OP_SUBASSIGN
               | OP_SUBREF
               | OP_TAN
               | OP_TANH
               | OP_TRANSPOSE
               | OP_TWICE
               | OP_VERTCAT
               | OP_VERTSPLIT
               deriving (Show, Eq)
instance Enum Operation where
        fromEnum (OP_ACOS) = 17
        fromEnum (OP_ACOSH) = 41
        fromEnum (OP_ADD) = 1
        fromEnum (OP_ADDNONZEROS) = 69
        fromEnum (OP_AND) = 24
        fromEnum (OP_ASIN) = 16
        fromEnum (OP_ASINH) = 40
        fromEnum (OP_ASSERTION) = 72
        fromEnum (OP_ASSIGN) = 0
        fromEnum (OP_ATAN) = 18
        fromEnum (OP_ATAN2) = 43
        fromEnum (OP_ATANH) = 42
        fromEnum (OP_BILIN) = 57
        fromEnum (OP_CALL) = 48
        fromEnum (OP_CEIL) = 27
        fromEnum (OP_CONST) = 44
        fromEnum (OP_CONSTPOW) = 9
        fromEnum (OP_COPYSIGN) = 31
        fromEnum (OP_COS) = 14
        fromEnum (OP_COSH) = 38
        fromEnum (OP_DETERMINANT) = 54
        fromEnum (OP_DIAGCAT) = 61
        fromEnum (OP_DIAGSPLIT) = 64
        fromEnum (OP_DIV) = 4
        fromEnum (OP_DOT) = 56
        fromEnum (OP_EQ) = 21
        fromEnum (OP_ERF) = 33
        fromEnum (OP_ERFINV) = 80
        fromEnum (OP_EXP) = 6
        fromEnum (OP_FABS) = 29
        fromEnum (OP_FIND) = 49
        fromEnum (OP_FLOOR) = 26
        fromEnum (OP_FMAX) = 35
        fromEnum (OP_FMIN) = 34
        fromEnum (OP_FMOD) = 28
        fromEnum (OP_GETNONZEROS) = 68
        fromEnum (OP_HORZCAT) = 59
        fromEnum (OP_HORZREPMAT) = 78
        fromEnum (OP_HORZREPSUM) = 79
        fromEnum (OP_HORZSPLIT) = 62
        fromEnum (OP_IF_ELSE_ZERO) = 32
        fromEnum (OP_INPUT) = 45
        fromEnum (OP_INV) = 36
        fromEnum (OP_INVERSE) = 55
        fromEnum (OP_LE) = 20
        fromEnum (OP_LIFT) = 82
        fromEnum (OP_LOG) = 7
        fromEnum (OP_LT) = 19
        fromEnum (OP_MAP) = 50
        fromEnum (OP_MONITOR) = 73
        fromEnum (OP_MTIMES) = 51
        fromEnum (OP_MUL) = 3
        fromEnum (OP_NE) = 22
        fromEnum (OP_NEG) = 5
        fromEnum (OP_NORM1) = 75
        fromEnum (OP_NORM2) = 74
        fromEnum (OP_NORMF) = 77
        fromEnum (OP_NORMINF) = 76
        fromEnum (OP_NOT) = 23
        fromEnum (OP_OR) = 25
        fromEnum (OP_OUTPUT) = 46
        fromEnum (OP_PARAMETER) = 47
        fromEnum (OP_POW) = 8
        fromEnum (OP_PRINTME) = 81
        fromEnum (OP_PROJECT) = 71
        fromEnum (OP_RANK1) = 58
        fromEnum (OP_RESHAPE) = 65
        fromEnum (OP_SETNONZEROS) = 70
        fromEnum (OP_SIGN) = 30
        fromEnum (OP_SIN) = 13
        fromEnum (OP_SINH) = 37
        fromEnum (OP_SOLVE) = 52
        fromEnum (OP_SQ) = 11
        fromEnum (OP_SQRT) = 10
        fromEnum (OP_SUB) = 2
        fromEnum (OP_SUBASSIGN) = 67
        fromEnum (OP_SUBREF) = 66
        fromEnum (OP_TAN) = 15
        fromEnum (OP_TANH) = 39
        fromEnum (OP_TRANSPOSE) = 53
        fromEnum (OP_TWICE) = 12
        fromEnum (OP_VERTCAT) = 60
        fromEnum (OP_VERTSPLIT) = 63
        toEnum (17) = OP_ACOS
        toEnum (41) = OP_ACOSH
        toEnum (1) = OP_ADD
        toEnum (69) = OP_ADDNONZEROS
        toEnum (24) = OP_AND
        toEnum (16) = OP_ASIN
        toEnum (40) = OP_ASINH
        toEnum (72) = OP_ASSERTION
        toEnum (0) = OP_ASSIGN
        toEnum (18) = OP_ATAN
        toEnum (43) = OP_ATAN2
        toEnum (42) = OP_ATANH
        toEnum (57) = OP_BILIN
        toEnum (48) = OP_CALL
        toEnum (27) = OP_CEIL
        toEnum (44) = OP_CONST
        toEnum (9) = OP_CONSTPOW
        toEnum (31) = OP_COPYSIGN
        toEnum (14) = OP_COS
        toEnum (38) = OP_COSH
        toEnum (54) = OP_DETERMINANT
        toEnum (61) = OP_DIAGCAT
        toEnum (64) = OP_DIAGSPLIT
        toEnum (4) = OP_DIV
        toEnum (56) = OP_DOT
        toEnum (21) = OP_EQ
        toEnum (33) = OP_ERF
        toEnum (80) = OP_ERFINV
        toEnum (6) = OP_EXP
        toEnum (29) = OP_FABS
        toEnum (49) = OP_FIND
        toEnum (26) = OP_FLOOR
        toEnum (35) = OP_FMAX
        toEnum (34) = OP_FMIN
        toEnum (28) = OP_FMOD
        toEnum (68) = OP_GETNONZEROS
        toEnum (59) = OP_HORZCAT
        toEnum (78) = OP_HORZREPMAT
        toEnum (79) = OP_HORZREPSUM
        toEnum (62) = OP_HORZSPLIT
        toEnum (32) = OP_IF_ELSE_ZERO
        toEnum (45) = OP_INPUT
        toEnum (36) = OP_INV
        toEnum (55) = OP_INVERSE
        toEnum (20) = OP_LE
        toEnum (82) = OP_LIFT
        toEnum (7) = OP_LOG
        toEnum (19) = OP_LT
        toEnum (50) = OP_MAP
        toEnum (73) = OP_MONITOR
        toEnum (51) = OP_MTIMES
        toEnum (3) = OP_MUL
        toEnum (22) = OP_NE
        toEnum (5) = OP_NEG
        toEnum (75) = OP_NORM1
        toEnum (74) = OP_NORM2
        toEnum (77) = OP_NORMF
        toEnum (76) = OP_NORMINF
        toEnum (23) = OP_NOT
        toEnum (25) = OP_OR
        toEnum (46) = OP_OUTPUT
        toEnum (47) = OP_PARAMETER
        toEnum (8) = OP_POW
        toEnum (81) = OP_PRINTME
        toEnum (71) = OP_PROJECT
        toEnum (58) = OP_RANK1
        toEnum (65) = OP_RESHAPE
        toEnum (70) = OP_SETNONZEROS
        toEnum (30) = OP_SIGN
        toEnum (13) = OP_SIN
        toEnum (37) = OP_SINH
        toEnum (52) = OP_SOLVE
        toEnum (11) = OP_SQ
        toEnum (10) = OP_SQRT
        toEnum (2) = OP_SUB
        toEnum (67) = OP_SUBASSIGN
        toEnum (66) = OP_SUBREF
        toEnum (15) = OP_TAN
        toEnum (39) = OP_TANH
        toEnum (53) = OP_TRANSPOSE
        toEnum (12) = OP_TWICE
        toEnum (60) = OP_VERTCAT
        toEnum (63) = OP_VERTSPLIT
        toEnum k
          = error $ "Operation: toEnum: got unhandled number: " ++ show k
instance Marshal Operation CInt where
        marshal = return . fromIntegral . fromEnum
instance WrapReturn CInt Operation where
        wrapReturn = return . toEnum . fromIntegral

-- EnumDecl: TypeID
data TypeID = OT_BOOL
            | OT_BOOLVECTOR
            | OT_DICT
            | OT_DOUBLE
            | OT_DOUBLEVECTOR
            | OT_FUNCTION
            | OT_INT
            | OT_INTVECTOR
            | OT_INTVECTORVECTOR
            | OT_NULL
            | OT_STRING
            | OT_STRINGVECTOR
            | OT_UNKNOWN
            | OT_VOIDPTR
            deriving (Show, Eq)
instance Enum TypeID where
        fromEnum (OT_BOOL) = 1
        fromEnum (OT_BOOLVECTOR) = 7
        fromEnum (OT_DICT) = 10
        fromEnum (OT_DOUBLE) = 3
        fromEnum (OT_DOUBLEVECTOR) = 8
        fromEnum (OT_FUNCTION) = 11
        fromEnum (OT_INT) = 2
        fromEnum (OT_INTVECTOR) = 5
        fromEnum (OT_INTVECTORVECTOR) = 6
        fromEnum (OT_NULL) = 0
        fromEnum (OT_STRING) = 4
        fromEnum (OT_STRINGVECTOR) = 9
        fromEnum (OT_UNKNOWN) = 13
        fromEnum (OT_VOIDPTR) = 12
        toEnum (1) = OT_BOOL
        toEnum (7) = OT_BOOLVECTOR
        toEnum (10) = OT_DICT
        toEnum (3) = OT_DOUBLE
        toEnum (8) = OT_DOUBLEVECTOR
        toEnum (11) = OT_FUNCTION
        toEnum (2) = OT_INT
        toEnum (5) = OT_INTVECTOR
        toEnum (6) = OT_INTVECTORVECTOR
        toEnum (0) = OT_NULL
        toEnum (4) = OT_STRING
        toEnum (9) = OT_STRINGVECTOR
        toEnum (13) = OT_UNKNOWN
        toEnum (12) = OT_VOIDPTR
        toEnum k
          = error $ "TypeID: toEnum: got unhandled number: " ++ show k
instance Marshal TypeID CInt where
        marshal = return . fromIntegral . fromEnum
instance WrapReturn CInt TypeID where
        wrapReturn = return . toEnum . fromIntegral

-- EnumDecl: Variability
data Variability = CONSTANT
                 | CONTINUOUS
                 | DISCRETE
                 | PARAMETER
                 deriving (Show, Eq)
instance Enum Variability where
        fromEnum (CONSTANT) = 0
        fromEnum (CONTINUOUS) = 3
        fromEnum (DISCRETE) = 2
        fromEnum (PARAMETER) = 1
        toEnum (0) = CONSTANT
        toEnum (3) = CONTINUOUS
        toEnum (2) = DISCRETE
        toEnum (1) = PARAMETER
        toEnum k
          = error $ "Variability: toEnum: got unhandled number: " ++ show k
instance Marshal Variability CInt where
        marshal = return . fromIntegral . fromEnum
instance WrapReturn CInt Variability where
        wrapReturn = return . toEnum . fromIntegral

