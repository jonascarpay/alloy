{-# OPTIONS_GHC -w #-}
module Parser.Parser where

import Parser.Lexer (Token)
import qualified Parser.Lexer as Lex
import qualified Data.ByteString as BS
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,308) ([0,25920,0,6480,0,0,0,512,0,0,4096,0,0,0,0,0,0,63488,63,0,0,24,1024,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,1620,0,405,0,405,16384,101,0,0,0,0,0,0,0,0,21504,6,38144,1,38144,1,25920,0,32768,7,0,0,0,16384,0,0,0,0,0,0,405,16384,101,0,0,0,2,32768,0,0,2,0,0,8192,0,0,0,0,0,32768,7,2048,0,0,8192,0,0,1620,0,405,0,405,16384,101,0,0,0,0,0,0,0,2,0,0,64,0,0,0,0,0,0,0,128,0,0,32768,0,0,0,0,2,0,32,0,0,0,0,128,0,8192,0,0,0,8192,0,32768,0,0,0,8192,0,0,16384,0,0,0,0,0,0,1536,0,385,0,384,16384,96,0,96,4096,24,0,24,1024,6,0,6,33024,1,32768,1,24640,0,24576,0,6160,0,6144,0,1540,0,1536,0,385,0,384,16384,96,0,96,4096,24,0,7680,0,0,0,1920,2,0,0,33248,0,0,0,8312,0,0,0,2078,0,0,32768,999,0,0,57344,249,0,0,0,0,0,0,0,0,0,0,1536,0,0,0,384,0,0,0,0,0,0,0,8192,0,0,0,0,129,0,0,0,0,16384,101,20480,25,20480,25,21504,6,21504,6,38144,1,0,0,0,0,25920,0,6480,0,0,0,0,0,1620,0,405,0,0,5120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21504,6,38144,1,38144,1,25920,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,32768,0,38144,1,25920,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,16384,101,20480,25,20480,25,21504,6,0,0,0,0,0,0,16,0,25920,0,6480,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Exp","BinExp","AppExp","AccExp","Exp0","Prim","ListItems","ListItemsRev","Binding","Bindings","BindingsRev","CBindings","CBindingsRev","NameList","NameListRev","Block","Stmts","StmtsRev","Stmt","let","in","with","inherit","if","then","else","true","false","return","break","continue","var","'+'","'-'","'*'","'/'","'=='","'/='","'<'","'>'","'<='","'>='","'++'","'.'","';'","':'","','","'='","'@'","'('","')'","'['","']'","'{'","'}'","num","id","str","%eof"]
        bit_start = st Prelude.* 62
        bit_end = (st Prelude.+ 1) Prelude.* 62
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..61]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (23) = happyShift action_9
action_0 (25) = happyShift action_10
action_0 (27) = happyShift action_11
action_0 (30) = happyShift action_12
action_0 (31) = happyShift action_13
action_0 (53) = happyShift action_14
action_0 (55) = happyShift action_15
action_0 (57) = happyShift action_16
action_0 (60) = happyShift action_17
action_0 (61) = happyShift action_18
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (60) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (49) = happyShift action_19
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (62) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (36) = happyShift action_42
action_4 (37) = happyShift action_43
action_4 (38) = happyShift action_44
action_4 (39) = happyShift action_45
action_4 (40) = happyShift action_46
action_4 (41) = happyShift action_47
action_4 (42) = happyShift action_48
action_4 (43) = happyShift action_49
action_4 (44) = happyShift action_50
action_4 (45) = happyShift action_51
action_4 (46) = happyShift action_52
action_4 _ = happyReduce_8

action_5 (30) = happyShift action_12
action_5 (31) = happyShift action_13
action_5 (53) = happyShift action_14
action_5 (60) = happyShift action_41
action_5 (61) = happyShift action_18
action_5 (7) = happyGoto action_40
action_5 (8) = happyGoto action_7
action_5 (9) = happyGoto action_8
action_5 _ = happyReduce_20

action_6 (47) = happyShift action_39
action_6 _ = happyReduce_22

action_7 _ = happyReduce_24

action_8 _ = happyReduce_27

action_9 (13) = happyGoto action_37
action_9 (14) = happyGoto action_38
action_9 _ = happyReduce_39

action_10 (23) = happyShift action_9
action_10 (25) = happyShift action_10
action_10 (27) = happyShift action_11
action_10 (30) = happyShift action_12
action_10 (31) = happyShift action_13
action_10 (53) = happyShift action_14
action_10 (55) = happyShift action_15
action_10 (57) = happyShift action_16
action_10 (60) = happyShift action_17
action_10 (61) = happyShift action_18
action_10 (4) = happyGoto action_36
action_10 (5) = happyGoto action_4
action_10 (6) = happyGoto action_5
action_10 (7) = happyGoto action_6
action_10 (8) = happyGoto action_7
action_10 (9) = happyGoto action_8
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (23) = happyShift action_9
action_11 (25) = happyShift action_10
action_11 (27) = happyShift action_11
action_11 (30) = happyShift action_12
action_11 (31) = happyShift action_13
action_11 (53) = happyShift action_14
action_11 (55) = happyShift action_15
action_11 (57) = happyShift action_16
action_11 (60) = happyShift action_17
action_11 (61) = happyShift action_18
action_11 (4) = happyGoto action_35
action_11 (5) = happyGoto action_4
action_11 (6) = happyGoto action_5
action_11 (7) = happyGoto action_6
action_11 (8) = happyGoto action_7
action_11 (9) = happyGoto action_8
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_29

action_13 _ = happyReduce_30

action_14 (23) = happyShift action_9
action_14 (25) = happyShift action_10
action_14 (27) = happyShift action_11
action_14 (30) = happyShift action_12
action_14 (31) = happyShift action_13
action_14 (53) = happyShift action_14
action_14 (55) = happyShift action_15
action_14 (57) = happyShift action_16
action_14 (60) = happyShift action_17
action_14 (61) = happyShift action_18
action_14 (4) = happyGoto action_34
action_14 (5) = happyGoto action_4
action_14 (6) = happyGoto action_5
action_14 (7) = happyGoto action_6
action_14 (8) = happyGoto action_7
action_14 (9) = happyGoto action_8
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (23) = happyShift action_9
action_15 (25) = happyShift action_10
action_15 (27) = happyShift action_11
action_15 (30) = happyShift action_12
action_15 (31) = happyShift action_13
action_15 (53) = happyShift action_14
action_15 (55) = happyShift action_15
action_15 (57) = happyShift action_16
action_15 (60) = happyShift action_17
action_15 (61) = happyShift action_18
action_15 (4) = happyGoto action_31
action_15 (5) = happyGoto action_4
action_15 (6) = happyGoto action_5
action_15 (7) = happyGoto action_6
action_15 (8) = happyGoto action_7
action_15 (9) = happyGoto action_8
action_15 (10) = happyGoto action_32
action_15 (11) = happyGoto action_33
action_15 _ = happyReduce_32

action_16 (32) = happyShift action_26
action_16 (33) = happyShift action_27
action_16 (34) = happyShift action_28
action_16 (35) = happyShift action_29
action_16 (60) = happyShift action_30
action_16 (15) = happyGoto action_20
action_16 (16) = happyGoto action_21
action_16 (19) = happyGoto action_22
action_16 (20) = happyGoto action_23
action_16 (21) = happyGoto action_24
action_16 (22) = happyGoto action_25
action_16 _ = happyReduce_42

action_17 (49) = happyShift action_19
action_17 _ = happyReduce_26

action_18 _ = happyReduce_28

action_19 (23) = happyShift action_9
action_19 (25) = happyShift action_10
action_19 (27) = happyShift action_11
action_19 (30) = happyShift action_12
action_19 (31) = happyShift action_13
action_19 (53) = happyShift action_14
action_19 (55) = happyShift action_15
action_19 (57) = happyShift action_16
action_19 (60) = happyShift action_17
action_19 (61) = happyShift action_18
action_19 (4) = happyGoto action_83
action_19 (5) = happyGoto action_4
action_19 (6) = happyGoto action_5
action_19 (7) = happyGoto action_6
action_19 (8) = happyGoto action_7
action_19 (9) = happyGoto action_8
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (58) = happyShift action_82
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (26) = happyShift action_66
action_21 (60) = happyShift action_67
action_21 (12) = happyGoto action_81
action_21 _ = happyReduce_41

action_22 (58) = happyShift action_80
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_47

action_24 (32) = happyShift action_26
action_24 (33) = happyShift action_27
action_24 (34) = happyShift action_28
action_24 (35) = happyShift action_29
action_24 (60) = happyShift action_30
action_24 (22) = happyGoto action_79
action_24 _ = happyReduce_48

action_25 (48) = happyShift action_78
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (23) = happyShift action_9
action_26 (25) = happyShift action_10
action_26 (27) = happyShift action_11
action_26 (30) = happyShift action_12
action_26 (31) = happyShift action_13
action_26 (53) = happyShift action_14
action_26 (55) = happyShift action_15
action_26 (57) = happyShift action_16
action_26 (60) = happyShift action_17
action_26 (61) = happyShift action_18
action_26 (4) = happyGoto action_77
action_26 (5) = happyGoto action_4
action_26 (6) = happyGoto action_5
action_26 (7) = happyGoto action_6
action_26 (8) = happyGoto action_7
action_26 (9) = happyGoto action_8
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (23) = happyShift action_9
action_27 (25) = happyShift action_10
action_27 (27) = happyShift action_11
action_27 (30) = happyShift action_12
action_27 (31) = happyShift action_13
action_27 (53) = happyShift action_14
action_27 (55) = happyShift action_15
action_27 (57) = happyShift action_16
action_27 (60) = happyShift action_17
action_27 (61) = happyShift action_18
action_27 (4) = happyGoto action_76
action_27 (5) = happyGoto action_4
action_27 (6) = happyGoto action_5
action_27 (7) = happyGoto action_6
action_27 (8) = happyGoto action_7
action_27 (9) = happyGoto action_8
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_56

action_29 (60) = happyShift action_75
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (51) = happyShift action_74
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_33

action_32 (56) = happyShift action_73
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (50) = happyShift action_72
action_33 _ = happyReduce_31

action_34 (54) = happyShift action_71
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (28) = happyShift action_70
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (48) = happyShift action_69
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (24) = happyShift action_68
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (26) = happyShift action_66
action_38 (60) = happyShift action_67
action_38 (12) = happyGoto action_65
action_38 _ = happyReduce_38

action_39 (60) = happyShift action_64
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (47) = happyShift action_39
action_40 _ = happyReduce_21

action_41 _ = happyReduce_26

action_42 (30) = happyShift action_12
action_42 (31) = happyShift action_13
action_42 (53) = happyShift action_14
action_42 (60) = happyShift action_41
action_42 (61) = happyShift action_18
action_42 (5) = happyGoto action_63
action_42 (6) = happyGoto action_5
action_42 (7) = happyGoto action_6
action_42 (8) = happyGoto action_7
action_42 (9) = happyGoto action_8
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (30) = happyShift action_12
action_43 (31) = happyShift action_13
action_43 (53) = happyShift action_14
action_43 (60) = happyShift action_41
action_43 (61) = happyShift action_18
action_43 (5) = happyGoto action_62
action_43 (6) = happyGoto action_5
action_43 (7) = happyGoto action_6
action_43 (8) = happyGoto action_7
action_43 (9) = happyGoto action_8
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (30) = happyShift action_12
action_44 (31) = happyShift action_13
action_44 (53) = happyShift action_14
action_44 (60) = happyShift action_41
action_44 (61) = happyShift action_18
action_44 (5) = happyGoto action_61
action_44 (6) = happyGoto action_5
action_44 (7) = happyGoto action_6
action_44 (8) = happyGoto action_7
action_44 (9) = happyGoto action_8
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (30) = happyShift action_12
action_45 (31) = happyShift action_13
action_45 (53) = happyShift action_14
action_45 (60) = happyShift action_41
action_45 (61) = happyShift action_18
action_45 (5) = happyGoto action_60
action_45 (6) = happyGoto action_5
action_45 (7) = happyGoto action_6
action_45 (8) = happyGoto action_7
action_45 (9) = happyGoto action_8
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (30) = happyShift action_12
action_46 (31) = happyShift action_13
action_46 (53) = happyShift action_14
action_46 (60) = happyShift action_41
action_46 (61) = happyShift action_18
action_46 (5) = happyGoto action_59
action_46 (6) = happyGoto action_5
action_46 (7) = happyGoto action_6
action_46 (8) = happyGoto action_7
action_46 (9) = happyGoto action_8
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (30) = happyShift action_12
action_47 (31) = happyShift action_13
action_47 (53) = happyShift action_14
action_47 (60) = happyShift action_41
action_47 (61) = happyShift action_18
action_47 (5) = happyGoto action_58
action_47 (6) = happyGoto action_5
action_47 (7) = happyGoto action_6
action_47 (8) = happyGoto action_7
action_47 (9) = happyGoto action_8
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (30) = happyShift action_12
action_48 (31) = happyShift action_13
action_48 (53) = happyShift action_14
action_48 (60) = happyShift action_41
action_48 (61) = happyShift action_18
action_48 (5) = happyGoto action_57
action_48 (6) = happyGoto action_5
action_48 (7) = happyGoto action_6
action_48 (8) = happyGoto action_7
action_48 (9) = happyGoto action_8
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (30) = happyShift action_12
action_49 (31) = happyShift action_13
action_49 (53) = happyShift action_14
action_49 (60) = happyShift action_41
action_49 (61) = happyShift action_18
action_49 (5) = happyGoto action_56
action_49 (6) = happyGoto action_5
action_49 (7) = happyGoto action_6
action_49 (8) = happyGoto action_7
action_49 (9) = happyGoto action_8
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (30) = happyShift action_12
action_50 (31) = happyShift action_13
action_50 (53) = happyShift action_14
action_50 (60) = happyShift action_41
action_50 (61) = happyShift action_18
action_50 (5) = happyGoto action_55
action_50 (6) = happyGoto action_5
action_50 (7) = happyGoto action_6
action_50 (8) = happyGoto action_7
action_50 (9) = happyGoto action_8
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (30) = happyShift action_12
action_51 (31) = happyShift action_13
action_51 (53) = happyShift action_14
action_51 (60) = happyShift action_41
action_51 (61) = happyShift action_18
action_51 (5) = happyGoto action_54
action_51 (6) = happyGoto action_5
action_51 (7) = happyGoto action_6
action_51 (8) = happyGoto action_7
action_51 (9) = happyGoto action_8
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (30) = happyShift action_12
action_52 (31) = happyShift action_13
action_52 (53) = happyShift action_14
action_52 (60) = happyShift action_41
action_52 (61) = happyShift action_18
action_52 (5) = happyGoto action_53
action_52 (6) = happyGoto action_5
action_52 (7) = happyGoto action_6
action_52 (8) = happyGoto action_7
action_52 (9) = happyGoto action_8
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (36) = happyShift action_42
action_53 (37) = happyShift action_43
action_53 (38) = happyShift action_44
action_53 (39) = happyShift action_45
action_53 _ = happyReduce_15

action_54 (36) = happyShift action_42
action_54 (37) = happyShift action_43
action_54 (38) = happyShift action_44
action_54 (39) = happyShift action_45
action_54 (42) = happyFail []
action_54 (43) = happyFail []
action_54 (44) = happyFail []
action_54 (45) = happyFail []
action_54 (46) = happyShift action_52
action_54 _ = happyReduce_13

action_55 (36) = happyShift action_42
action_55 (37) = happyShift action_43
action_55 (38) = happyShift action_44
action_55 (39) = happyShift action_45
action_55 (42) = happyFail []
action_55 (43) = happyFail []
action_55 (44) = happyFail []
action_55 (45) = happyFail []
action_55 (46) = happyShift action_52
action_55 _ = happyReduce_14

action_56 (36) = happyShift action_42
action_56 (37) = happyShift action_43
action_56 (38) = happyShift action_44
action_56 (39) = happyShift action_45
action_56 (42) = happyFail []
action_56 (43) = happyFail []
action_56 (44) = happyFail []
action_56 (45) = happyFail []
action_56 (46) = happyShift action_52
action_56 _ = happyReduce_11

action_57 (36) = happyShift action_42
action_57 (37) = happyShift action_43
action_57 (38) = happyShift action_44
action_57 (39) = happyShift action_45
action_57 (42) = happyFail []
action_57 (43) = happyFail []
action_57 (44) = happyFail []
action_57 (45) = happyFail []
action_57 (46) = happyShift action_52
action_57 _ = happyReduce_12

action_58 (36) = happyShift action_42
action_58 (37) = happyShift action_43
action_58 (38) = happyShift action_44
action_58 (39) = happyShift action_45
action_58 (40) = happyFail []
action_58 (41) = happyFail []
action_58 (42) = happyShift action_48
action_58 (43) = happyShift action_49
action_58 (44) = happyShift action_50
action_58 (45) = happyShift action_51
action_58 (46) = happyShift action_52
action_58 _ = happyReduce_10

action_59 (36) = happyShift action_42
action_59 (37) = happyShift action_43
action_59 (38) = happyShift action_44
action_59 (39) = happyShift action_45
action_59 (40) = happyFail []
action_59 (41) = happyFail []
action_59 (42) = happyShift action_48
action_59 (43) = happyShift action_49
action_59 (44) = happyShift action_50
action_59 (45) = happyShift action_51
action_59 (46) = happyShift action_52
action_59 _ = happyReduce_9

action_60 _ = happyReduce_19

action_61 _ = happyReduce_18

action_62 (38) = happyShift action_44
action_62 (39) = happyShift action_45
action_62 _ = happyReduce_17

action_63 (38) = happyShift action_44
action_63 (39) = happyShift action_45
action_63 _ = happyReduce_16

action_64 _ = happyReduce_23

action_65 (48) = happyShift action_97
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (53) = happyShift action_95
action_66 (60) = happyShift action_96
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (17) = happyGoto action_93
action_67 (18) = happyGoto action_94
action_67 _ = happyReduce_45

action_68 (23) = happyShift action_9
action_68 (25) = happyShift action_10
action_68 (27) = happyShift action_11
action_68 (30) = happyShift action_12
action_68 (31) = happyShift action_13
action_68 (53) = happyShift action_14
action_68 (55) = happyShift action_15
action_68 (57) = happyShift action_16
action_68 (60) = happyShift action_17
action_68 (61) = happyShift action_18
action_68 (4) = happyGoto action_92
action_68 (5) = happyGoto action_4
action_68 (6) = happyGoto action_5
action_68 (7) = happyGoto action_6
action_68 (8) = happyGoto action_7
action_68 (9) = happyGoto action_8
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (23) = happyShift action_9
action_69 (25) = happyShift action_10
action_69 (27) = happyShift action_11
action_69 (30) = happyShift action_12
action_69 (31) = happyShift action_13
action_69 (53) = happyShift action_14
action_69 (55) = happyShift action_15
action_69 (57) = happyShift action_16
action_69 (60) = happyShift action_17
action_69 (61) = happyShift action_18
action_69 (4) = happyGoto action_91
action_69 (5) = happyGoto action_4
action_69 (6) = happyGoto action_5
action_69 (7) = happyGoto action_6
action_69 (8) = happyGoto action_7
action_69 (9) = happyGoto action_8
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (23) = happyShift action_9
action_70 (25) = happyShift action_10
action_70 (27) = happyShift action_11
action_70 (30) = happyShift action_12
action_70 (31) = happyShift action_13
action_70 (53) = happyShift action_14
action_70 (55) = happyShift action_15
action_70 (57) = happyShift action_16
action_70 (60) = happyShift action_17
action_70 (61) = happyShift action_18
action_70 (4) = happyGoto action_90
action_70 (5) = happyGoto action_4
action_70 (6) = happyGoto action_5
action_70 (7) = happyGoto action_6
action_70 (8) = happyGoto action_7
action_70 (9) = happyGoto action_8
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_25

action_72 (23) = happyShift action_9
action_72 (25) = happyShift action_10
action_72 (27) = happyShift action_11
action_72 (30) = happyShift action_12
action_72 (31) = happyShift action_13
action_72 (53) = happyShift action_14
action_72 (55) = happyShift action_15
action_72 (57) = happyShift action_16
action_72 (60) = happyShift action_17
action_72 (61) = happyShift action_18
action_72 (4) = happyGoto action_89
action_72 (5) = happyGoto action_4
action_72 (6) = happyGoto action_5
action_72 (7) = happyGoto action_6
action_72 (8) = happyGoto action_7
action_72 (9) = happyGoto action_8
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_7

action_74 (23) = happyShift action_9
action_74 (25) = happyShift action_10
action_74 (27) = happyShift action_11
action_74 (30) = happyShift action_12
action_74 (31) = happyShift action_13
action_74 (53) = happyShift action_14
action_74 (55) = happyShift action_15
action_74 (57) = happyShift action_16
action_74 (60) = happyShift action_17
action_74 (61) = happyShift action_18
action_74 (4) = happyGoto action_88
action_74 (5) = happyGoto action_4
action_74 (6) = happyGoto action_5
action_74 (7) = happyGoto action_6
action_74 (8) = happyGoto action_7
action_74 (9) = happyGoto action_8
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (49) = happyShift action_86
action_75 (51) = happyShift action_87
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_55

action_77 _ = happyReduce_51

action_78 _ = happyReduce_49

action_79 (48) = happyShift action_85
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_6

action_81 (50) = happyShift action_84
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_5

action_83 _ = happyReduce_1

action_84 _ = happyReduce_43

action_85 _ = happyReduce_50

action_86 (23) = happyShift action_9
action_86 (25) = happyShift action_10
action_86 (27) = happyShift action_11
action_86 (30) = happyShift action_12
action_86 (31) = happyShift action_13
action_86 (53) = happyShift action_14
action_86 (55) = happyShift action_15
action_86 (57) = happyShift action_16
action_86 (60) = happyShift action_17
action_86 (61) = happyShift action_18
action_86 (4) = happyGoto action_103
action_86 (5) = happyGoto action_4
action_86 (6) = happyGoto action_5
action_86 (7) = happyGoto action_6
action_86 (8) = happyGoto action_7
action_86 (9) = happyGoto action_8
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (23) = happyShift action_9
action_87 (25) = happyShift action_10
action_87 (27) = happyShift action_11
action_87 (30) = happyShift action_12
action_87 (31) = happyShift action_13
action_87 (53) = happyShift action_14
action_87 (55) = happyShift action_15
action_87 (57) = happyShift action_16
action_87 (60) = happyShift action_17
action_87 (61) = happyShift action_18
action_87 (4) = happyGoto action_102
action_87 (5) = happyGoto action_4
action_87 (6) = happyGoto action_5
action_87 (7) = happyGoto action_6
action_87 (8) = happyGoto action_7
action_87 (9) = happyGoto action_8
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_54

action_89 _ = happyReduce_34

action_90 (29) = happyShift action_101
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_3

action_92 _ = happyReduce_2

action_93 (51) = happyShift action_100
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (60) = happyShift action_99
action_94 _ = happyReduce_44

action_95 (23) = happyShift action_9
action_95 (25) = happyShift action_10
action_95 (27) = happyShift action_11
action_95 (30) = happyShift action_12
action_95 (31) = happyShift action_13
action_95 (53) = happyShift action_14
action_95 (55) = happyShift action_15
action_95 (57) = happyShift action_16
action_95 (60) = happyShift action_17
action_95 (61) = happyShift action_18
action_95 (4) = happyGoto action_98
action_95 (5) = happyGoto action_4
action_95 (6) = happyGoto action_5
action_95 (7) = happyGoto action_6
action_95 (8) = happyGoto action_7
action_95 (9) = happyGoto action_8
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_36

action_97 _ = happyReduce_40

action_98 (54) = happyShift action_107
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_46

action_100 (23) = happyShift action_9
action_100 (25) = happyShift action_10
action_100 (27) = happyShift action_11
action_100 (30) = happyShift action_12
action_100 (31) = happyShift action_13
action_100 (53) = happyShift action_14
action_100 (55) = happyShift action_15
action_100 (57) = happyShift action_16
action_100 (60) = happyShift action_17
action_100 (61) = happyShift action_18
action_100 (4) = happyGoto action_106
action_100 (5) = happyGoto action_4
action_100 (6) = happyGoto action_5
action_100 (7) = happyGoto action_6
action_100 (8) = happyGoto action_7
action_100 (9) = happyGoto action_8
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (23) = happyShift action_9
action_101 (25) = happyShift action_10
action_101 (27) = happyShift action_11
action_101 (30) = happyShift action_12
action_101 (31) = happyShift action_13
action_101 (53) = happyShift action_14
action_101 (55) = happyShift action_15
action_101 (57) = happyShift action_16
action_101 (60) = happyShift action_17
action_101 (61) = happyShift action_18
action_101 (4) = happyGoto action_105
action_101 (5) = happyGoto action_4
action_101 (6) = happyGoto action_5
action_101 (7) = happyGoto action_6
action_101 (8) = happyGoto action_7
action_101 (9) = happyGoto action_8
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_52

action_103 (51) = happyShift action_104
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (23) = happyShift action_9
action_104 (25) = happyShift action_10
action_104 (27) = happyShift action_11
action_104 (30) = happyShift action_12
action_104 (31) = happyShift action_13
action_104 (53) = happyShift action_14
action_104 (55) = happyShift action_15
action_104 (57) = happyShift action_16
action_104 (60) = happyShift action_17
action_104 (61) = happyShift action_18
action_104 (4) = happyGoto action_109
action_104 (5) = happyGoto action_4
action_104 (6) = happyGoto action_5
action_104 (7) = happyGoto action_6
action_104 (8) = happyGoto action_7
action_104 (9) = happyGoto action_8
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_4

action_106 _ = happyReduce_35

action_107 (17) = happyGoto action_108
action_107 (18) = happyGoto action_94
action_107 _ = happyReduce_45

action_108 _ = happyReduce_37

action_109 _ = happyReduce_53

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (Lex.Ident happy_var_1))
	 =  HappyAbsSyn4
		 (Lam happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 4 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (With happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 4 happyReduction_4
happyReduction_4 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Cond happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Attr happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (BlockExpr happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (List happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (CompOp Eq) happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (CompOp Neq) happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (CompOp Gt) happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (CompOp Lt) happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (CompOp Geq) happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (CompOp Leq) happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp ConcatOp happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (ArithOp Add) happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  5 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (ArithOp Sub) happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (ArithOp Mul) happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (BinExp (ArithOp Div) happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  5 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  6 happyReduction_21
happyReduction_21 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (App happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  6 happyReduction_22
happyReduction_22 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  7 happyReduction_23
happyReduction_23 (HappyTerminal (Lex.Ident happy_var_3))
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Acc happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  7 happyReduction_24
happyReduction_24 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  8 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  8 happyReduction_26
happyReduction_26 (HappyTerminal (Lex.Ident happy_var_1))
	 =  HappyAbsSyn8
		 (Var happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  8 happyReduction_27
happyReduction_27 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 (HappyTerminal (Lex.String happy_var_1))
	 =  HappyAbsSyn9
		 (PString happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn9
		 (PBool True
	)

happyReduce_30 = happySpecReduce_1  9 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn9
		 (PBool False
	)

happyReduce_31 = happySpecReduce_1  10 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (reverse happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  10 happyReduction_32
happyReduction_32  =  HappyAbsSyn10
		 ([]
	)

happyReduce_33 = happySpecReduce_1  11 happyReduction_33
happyReduction_33 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  11 happyReduction_34
happyReduction_34 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 4 12 happyReduction_35
happyReduction_35 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	(HappyTerminal (Lex.Ident happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Binding happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_2  12 happyReduction_36
happyReduction_36 (HappyTerminal (Lex.Ident happy_var_2))
	_
	 =  HappyAbsSyn12
		 (Inherit happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 5 12 happyReduction_37
happyReduction_37 ((HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (InheritFrom happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_1  13 happyReduction_38
happyReduction_38 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (reverse happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  14 happyReduction_39
happyReduction_39  =  HappyAbsSyn14
		 ([]
	)

happyReduce_40 = happySpecReduce_3  14 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_2 : happy_var_1
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  15 happyReduction_41
happyReduction_41 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (reverse happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  16 happyReduction_42
happyReduction_42  =  HappyAbsSyn16
		 ([]
	)

happyReduce_43 = happySpecReduce_3  16 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_1
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  17 happyReduction_44
happyReduction_44 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (reverse happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  18 happyReduction_45
happyReduction_45  =  HappyAbsSyn18
		 ([]
	)

happyReduce_46 = happySpecReduce_2  18 happyReduction_46
happyReduction_46 (HappyTerminal (Lex.Ident happy_var_2))
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_2 : happy_var_1
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  19 happyReduction_47
happyReduction_47 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (Block happy_var_1 Nothing
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  20 happyReduction_48
happyReduction_48 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (reverse happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  21 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 ([ happy_var_1 ]
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  21 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_2 : happy_var_1
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  22 happyReduction_51
happyReduction_51 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (Return happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 4 22 happyReduction_52
happyReduction_52 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Lex.Ident happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Decl happy_var_2 Nothing happy_var_4
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 6 22 happyReduction_53
happyReduction_53 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Lex.Ident happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Decl happy_var_2 (Just happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_3  22 happyReduction_54
happyReduction_54 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (Lex.Ident happy_var_1))
	 =  HappyAbsSyn22
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  22 happyReduction_55
happyReduction_55 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (Break Nothing (Just happy_var_2)
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  22 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn22
		 (Continue Nothing
	)

happyNewToken action sts stk [] =
	action 62 62 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Lex.Let -> cont 23;
	Lex.In -> cont 24;
	Lex.With -> cont 25;
	Lex.Inherit -> cont 26;
	Lex.If -> cont 27;
	Lex.Then -> cont 28;
	Lex.Else -> cont 29;
	Lex.True -> cont 30;
	Lex.False -> cont 31;
	Lex.Return -> cont 32;
	Lex.Break -> cont 33;
	Lex.Continue -> cont 34;
	Lex.Var -> cont 35;
	Lex.Add -> cont 36;
	Lex.Sub -> cont 37;
	Lex.Mul -> cont 38;
	Lex.Div -> cont 39;
	Lex.Eq -> cont 40;
	Lex.Neq -> cont 41;
	Lex.Lt -> cont 42;
	Lex.Gt -> cont 43;
	Lex.Leq -> cont 44;
	Lex.Geq -> cont 45;
	Lex.Cat -> cont 46;
	Lex.Dot -> cont 47;
	Lex.Semicolon -> cont 48;
	Lex.Colon -> cont 49;
	Lex.Comma -> cont 50;
	Lex.Assign -> cont 51;
	Lex.At -> cont 52;
	Lex.LParen -> cont 53;
	Lex.RParen -> cont 54;
	Lex.LBrack -> cont 55;
	Lex.RBrack -> cont 56;
	Lex.LBrace -> cont 57;
	Lex.RBrace -> cont 58;
	Lex.Num happy_dollar_dollar -> cont 59;
	Lex.Ident happy_dollar_dollar -> cont 60;
	Lex.String happy_dollar_dollar -> cont 61;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 62 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Name = BS.ByteString

data Prim
  = PInt Int
  | PBool Bool
  | PString BS.ByteString
  deriving (Eq, Show)

data AST
  = Let [Binding] AST
  | Var Name
  | App AST AST
  | Lam Name AST
  | With AST AST
  | Cond AST AST AST
  | Attr [Binding]
  | List [AST]
  | Acc AST Name
  | BinExp BinOp AST AST
  | BlockExpr Block
  | Prim Prim
  deriving (Eq, Show)

data BinOp
  = ArithOp ArithOp
  | CompOp CompOp
  | ConcatOp
  deriving (Eq, Show)

data ArithOp = Add | Sub | Mul | Div
  deriving (Eq, Show)

data CompOp = Eq | Neq | Lt | Gt | Geq | Leq
  deriving (Eq, Show)

data Block = Block [Stmt] (Maybe AST)
  deriving (Eq, Show)

data Stmt
  = Return AST
  | Break (Maybe Name) (Maybe AST)
  | Continue (Maybe Name)
  | Assign Name AST
  | Decl Name (Maybe AST) AST
  deriving (Eq, Show)

data Binding
  = Binding Name [Name] AST -- with argument list
  | Inherit Name
  | InheritFrom AST [Name]
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError tkns = error $ "parse error hombre: " <> show tkns
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
