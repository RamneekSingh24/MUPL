{-# OPTIONS_GHC -w #-}
module Parser where
import Ast
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,315) ([8448,50242,777,4360,20002,8216,3959,0,16896,34948,1555,8720,40004,32816,8464,34018,33793,4360,3111,0,0,64,0,0,0,0,0,64,0,0,0,0,4,0,0,0,0,0,0,0,8192,0,0,0,0,512,0,128,0,0,2,47104,379,0,0,0,0,0,0,30720,247,0,32,0,0,0,0,2050,8721,6222,0,0,0,33858,5000,4102,17442,12444,4224,57889,388,2180,10001,8204,34884,24888,8448,50242,777,4360,20002,16408,4232,49777,16896,34948,1555,8720,40004,48,0,0,0,0,0,0,0,0,0,0,57344,224,0,1792,7,0,14392,0,0,448,0,2048,14,0,28736,0,49152,1979,0,256,0,0,0,0,16,0,0,16896,34948,1555,8720,40004,32816,8464,34018,1,0,128,0,0,64,0,8192,0,0,1536,30464,2063,0,31672,0,49152,5085,0,0,0,16,0,0,8,0,0,0,0,32,0,0,16390,4232,49777,0,0,0,32,16384,0,0,0,0,0,0,0,0,24,2,1024,0,0,16,0,0,48,31672,4,8192,0,64,0,0,96,8,4096,0,0,64,0,0,0,0,0,16384,4232,49777,0,0,64,0,0,12,7918,2048,33792,4360,3111,0,0,4,0,49152,57344,494,128,0,0,0,0,3072,4096,17442,12444,4224,57889,388,0,6144,8192,34884,24888,56832,61,0,61152,32769,0,0,0,0,0,0,56768,3,32769,8464,34018,30721,247,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Start","Decl","Type","Expr","\";\"","\"(\"","\")\"","\"+\"","\"-\"","\"*\"","\"~\"","\"=\"","\"<\"","\">\"","\"!\"","\"&&\"","\"||\"","\"^\"","\"=>\"","\"if\"","\"then\"","\"else\"","\"fi\"","\"let\"","\":=\"","\"in\"","\"end\"","\"false\"","\"true\"","\"fn\"","\":=>\"","\"->\"","\"fun\"","\"done\"","\"::\"","\"int\"","\"bool\"","int","var","%eof"]
        bit_start = st Prelude.* 43
        bit_end = (st Prelude.+ 1) Prelude.* 43
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..42]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (9) = happyShift action_3
action_0 (14) = happyShift action_4
action_0 (18) = happyShift action_5
action_0 (23) = happyShift action_6
action_0 (27) = happyShift action_7
action_0 (31) = happyShift action_8
action_0 (32) = happyShift action_9
action_0 (33) = happyShift action_10
action_0 (36) = happyShift action_11
action_0 (41) = happyShift action_12
action_0 (42) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (7) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (9) = happyShift action_3
action_1 (14) = happyShift action_4
action_1 (18) = happyShift action_5
action_1 (23) = happyShift action_6
action_1 (27) = happyShift action_7
action_1 (31) = happyShift action_8
action_1 (32) = happyShift action_9
action_1 (33) = happyShift action_10
action_1 (36) = happyShift action_11
action_1 (41) = happyShift action_12
action_1 (42) = happyShift action_13
action_1 (7) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (8) = happyShift action_26
action_2 (11) = happyShift action_27
action_2 (12) = happyShift action_28
action_2 (13) = happyShift action_29
action_2 (15) = happyShift action_30
action_2 (16) = happyShift action_31
action_2 (17) = happyShift action_32
action_2 (19) = happyShift action_33
action_2 (20) = happyShift action_34
action_2 (21) = happyShift action_35
action_2 (22) = happyShift action_36
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (9) = happyShift action_3
action_3 (14) = happyShift action_4
action_3 (18) = happyShift action_5
action_3 (23) = happyShift action_6
action_3 (27) = happyShift action_7
action_3 (31) = happyShift action_8
action_3 (32) = happyShift action_9
action_3 (33) = happyShift action_23
action_3 (36) = happyShift action_24
action_3 (41) = happyShift action_12
action_3 (42) = happyShift action_25
action_3 (7) = happyGoto action_22
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (9) = happyShift action_3
action_4 (14) = happyShift action_4
action_4 (18) = happyShift action_5
action_4 (23) = happyShift action_6
action_4 (27) = happyShift action_7
action_4 (31) = happyShift action_8
action_4 (32) = happyShift action_9
action_4 (33) = happyShift action_10
action_4 (36) = happyShift action_11
action_4 (41) = happyShift action_12
action_4 (42) = happyShift action_13
action_4 (7) = happyGoto action_21
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (9) = happyShift action_3
action_5 (14) = happyShift action_4
action_5 (18) = happyShift action_5
action_5 (23) = happyShift action_6
action_5 (27) = happyShift action_7
action_5 (31) = happyShift action_8
action_5 (32) = happyShift action_9
action_5 (33) = happyShift action_10
action_5 (36) = happyShift action_11
action_5 (41) = happyShift action_12
action_5 (42) = happyShift action_13
action_5 (7) = happyGoto action_20
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (9) = happyShift action_3
action_6 (14) = happyShift action_4
action_6 (18) = happyShift action_5
action_6 (23) = happyShift action_6
action_6 (27) = happyShift action_7
action_6 (31) = happyShift action_8
action_6 (32) = happyShift action_9
action_6 (33) = happyShift action_10
action_6 (36) = happyShift action_11
action_6 (41) = happyShift action_12
action_6 (42) = happyShift action_13
action_6 (7) = happyGoto action_19
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (42) = happyShift action_18
action_7 (5) = happyGoto action_17
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_26

action_9 _ = happyReduce_27

action_10 (9) = happyShift action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (42) = happyShift action_15
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_28

action_13 _ = happyReduce_29

action_14 (43) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (9) = happyShift action_55
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (42) = happyShift action_54
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (29) = happyShift action_53
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (28) = happyShift action_52
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (11) = happyShift action_27
action_19 (12) = happyShift action_28
action_19 (13) = happyShift action_29
action_19 (15) = happyShift action_30
action_19 (16) = happyShift action_31
action_19 (17) = happyShift action_32
action_19 (19) = happyShift action_33
action_19 (20) = happyShift action_34
action_19 (21) = happyShift action_35
action_19 (22) = happyShift action_36
action_19 (24) = happyShift action_51
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (22) = happyShift action_36
action_20 _ = happyReduce_14

action_21 (22) = happyShift action_36
action_21 _ = happyReduce_7

action_22 (10) = happyShift action_50
action_22 (11) = happyShift action_27
action_22 (12) = happyShift action_28
action_22 (13) = happyShift action_29
action_22 (15) = happyShift action_30
action_22 (16) = happyShift action_31
action_22 (17) = happyShift action_32
action_22 (19) = happyShift action_33
action_22 (20) = happyShift action_34
action_22 (21) = happyShift action_35
action_22 (22) = happyShift action_36
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (9) = happyShift action_49
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (42) = happyShift action_48
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (9) = happyShift action_3
action_25 (14) = happyShift action_4
action_25 (18) = happyShift action_5
action_25 (23) = happyShift action_6
action_25 (27) = happyShift action_7
action_25 (31) = happyShift action_8
action_25 (32) = happyShift action_9
action_25 (33) = happyShift action_10
action_25 (36) = happyShift action_11
action_25 (41) = happyShift action_12
action_25 (42) = happyShift action_13
action_25 (7) = happyGoto action_47
action_25 _ = happyReduce_29

action_26 _ = happyReduce_1

action_27 (9) = happyShift action_3
action_27 (14) = happyShift action_4
action_27 (18) = happyShift action_5
action_27 (23) = happyShift action_6
action_27 (27) = happyShift action_7
action_27 (31) = happyShift action_8
action_27 (32) = happyShift action_9
action_27 (33) = happyShift action_10
action_27 (36) = happyShift action_11
action_27 (41) = happyShift action_12
action_27 (42) = happyShift action_13
action_27 (7) = happyGoto action_46
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (9) = happyShift action_3
action_28 (14) = happyShift action_4
action_28 (18) = happyShift action_5
action_28 (23) = happyShift action_6
action_28 (27) = happyShift action_7
action_28 (31) = happyShift action_8
action_28 (32) = happyShift action_9
action_28 (33) = happyShift action_10
action_28 (36) = happyShift action_11
action_28 (41) = happyShift action_12
action_28 (42) = happyShift action_13
action_28 (7) = happyGoto action_45
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (9) = happyShift action_3
action_29 (14) = happyShift action_4
action_29 (18) = happyShift action_5
action_29 (23) = happyShift action_6
action_29 (27) = happyShift action_7
action_29 (31) = happyShift action_8
action_29 (32) = happyShift action_9
action_29 (33) = happyShift action_10
action_29 (36) = happyShift action_11
action_29 (41) = happyShift action_12
action_29 (42) = happyShift action_13
action_29 (7) = happyGoto action_44
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (9) = happyShift action_3
action_30 (14) = happyShift action_4
action_30 (18) = happyShift action_5
action_30 (23) = happyShift action_6
action_30 (27) = happyShift action_7
action_30 (31) = happyShift action_8
action_30 (32) = happyShift action_9
action_30 (33) = happyShift action_10
action_30 (36) = happyShift action_11
action_30 (41) = happyShift action_12
action_30 (42) = happyShift action_13
action_30 (7) = happyGoto action_43
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (9) = happyShift action_3
action_31 (14) = happyShift action_4
action_31 (18) = happyShift action_5
action_31 (23) = happyShift action_6
action_31 (27) = happyShift action_7
action_31 (31) = happyShift action_8
action_31 (32) = happyShift action_9
action_31 (33) = happyShift action_10
action_31 (36) = happyShift action_11
action_31 (41) = happyShift action_12
action_31 (42) = happyShift action_13
action_31 (7) = happyGoto action_42
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (9) = happyShift action_3
action_32 (14) = happyShift action_4
action_32 (18) = happyShift action_5
action_32 (23) = happyShift action_6
action_32 (27) = happyShift action_7
action_32 (31) = happyShift action_8
action_32 (32) = happyShift action_9
action_32 (33) = happyShift action_10
action_32 (36) = happyShift action_11
action_32 (41) = happyShift action_12
action_32 (42) = happyShift action_13
action_32 (7) = happyGoto action_41
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (9) = happyShift action_3
action_33 (14) = happyShift action_4
action_33 (18) = happyShift action_5
action_33 (23) = happyShift action_6
action_33 (27) = happyShift action_7
action_33 (31) = happyShift action_8
action_33 (32) = happyShift action_9
action_33 (33) = happyShift action_10
action_33 (36) = happyShift action_11
action_33 (41) = happyShift action_12
action_33 (42) = happyShift action_13
action_33 (7) = happyGoto action_40
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (9) = happyShift action_3
action_34 (14) = happyShift action_4
action_34 (18) = happyShift action_5
action_34 (23) = happyShift action_6
action_34 (27) = happyShift action_7
action_34 (31) = happyShift action_8
action_34 (32) = happyShift action_9
action_34 (33) = happyShift action_10
action_34 (36) = happyShift action_11
action_34 (41) = happyShift action_12
action_34 (42) = happyShift action_13
action_34 (7) = happyGoto action_39
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (9) = happyShift action_3
action_35 (14) = happyShift action_4
action_35 (18) = happyShift action_5
action_35 (23) = happyShift action_6
action_35 (27) = happyShift action_7
action_35 (31) = happyShift action_8
action_35 (32) = happyShift action_9
action_35 (33) = happyShift action_10
action_35 (36) = happyShift action_11
action_35 (41) = happyShift action_12
action_35 (42) = happyShift action_13
action_35 (7) = happyGoto action_38
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (9) = happyShift action_3
action_36 (14) = happyShift action_4
action_36 (18) = happyShift action_5
action_36 (23) = happyShift action_6
action_36 (27) = happyShift action_7
action_36 (31) = happyShift action_8
action_36 (32) = happyShift action_9
action_36 (33) = happyShift action_10
action_36 (36) = happyShift action_11
action_36 (41) = happyShift action_12
action_36 (42) = happyShift action_13
action_36 (7) = happyGoto action_37
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (11) = happyShift action_27
action_37 (12) = happyShift action_28
action_37 (13) = happyShift action_29
action_37 (15) = happyShift action_30
action_37 (16) = happyShift action_31
action_37 (17) = happyShift action_32
action_37 (19) = happyShift action_33
action_37 (20) = happyShift action_34
action_37 (21) = happyShift action_35
action_37 (22) = happyShift action_36
action_37 _ = happyReduce_18

action_38 (22) = happyShift action_36
action_38 _ = happyReduce_17

action_39 (22) = happyShift action_36
action_39 _ = happyReduce_16

action_40 (22) = happyShift action_36
action_40 _ = happyReduce_15

action_41 (11) = happyShift action_27
action_41 (12) = happyShift action_28
action_41 (13) = happyShift action_29
action_41 (15) = happyFail []
action_41 (16) = happyFail []
action_41 (17) = happyFail []
action_41 (19) = happyShift action_33
action_41 (20) = happyShift action_34
action_41 (21) = happyShift action_35
action_41 (22) = happyShift action_36
action_41 _ = happyReduce_13

action_42 (11) = happyShift action_27
action_42 (12) = happyShift action_28
action_42 (13) = happyShift action_29
action_42 (15) = happyFail []
action_42 (16) = happyFail []
action_42 (17) = happyFail []
action_42 (19) = happyShift action_33
action_42 (20) = happyShift action_34
action_42 (21) = happyShift action_35
action_42 (22) = happyShift action_36
action_42 _ = happyReduce_12

action_43 (11) = happyShift action_27
action_43 (12) = happyShift action_28
action_43 (13) = happyShift action_29
action_43 (15) = happyFail []
action_43 (16) = happyFail []
action_43 (17) = happyFail []
action_43 (19) = happyShift action_33
action_43 (20) = happyShift action_34
action_43 (21) = happyShift action_35
action_43 (22) = happyShift action_36
action_43 _ = happyReduce_11

action_44 (19) = happyShift action_33
action_44 (20) = happyShift action_34
action_44 (21) = happyShift action_35
action_44 (22) = happyShift action_36
action_44 _ = happyReduce_10

action_45 (13) = happyShift action_29
action_45 (19) = happyShift action_33
action_45 (20) = happyShift action_34
action_45 (21) = happyShift action_35
action_45 (22) = happyShift action_36
action_45 _ = happyReduce_9

action_46 (13) = happyShift action_29
action_46 (19) = happyShift action_33
action_46 (20) = happyShift action_34
action_46 (21) = happyShift action_35
action_46 (22) = happyShift action_36
action_46 _ = happyReduce_8

action_47 (10) = happyShift action_63
action_47 (11) = happyShift action_27
action_47 (12) = happyShift action_28
action_47 (13) = happyShift action_29
action_47 (15) = happyShift action_30
action_47 (16) = happyShift action_31
action_47 (17) = happyShift action_32
action_47 (19) = happyShift action_33
action_47 (20) = happyShift action_34
action_47 (21) = happyShift action_35
action_47 (22) = happyShift action_36
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (9) = happyShift action_62
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (42) = happyShift action_61
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_6

action_51 (9) = happyShift action_3
action_51 (14) = happyShift action_4
action_51 (18) = happyShift action_5
action_51 (23) = happyShift action_6
action_51 (27) = happyShift action_7
action_51 (31) = happyShift action_8
action_51 (32) = happyShift action_9
action_51 (33) = happyShift action_10
action_51 (36) = happyShift action_11
action_51 (41) = happyShift action_12
action_51 (42) = happyShift action_13
action_51 (7) = happyGoto action_60
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (9) = happyShift action_3
action_52 (14) = happyShift action_4
action_52 (18) = happyShift action_5
action_52 (23) = happyShift action_6
action_52 (27) = happyShift action_7
action_52 (31) = happyShift action_8
action_52 (32) = happyShift action_9
action_52 (33) = happyShift action_10
action_52 (36) = happyShift action_11
action_52 (41) = happyShift action_12
action_52 (42) = happyShift action_13
action_52 (7) = happyGoto action_59
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (9) = happyShift action_3
action_53 (14) = happyShift action_4
action_53 (18) = happyShift action_5
action_53 (23) = happyShift action_6
action_53 (27) = happyShift action_7
action_53 (31) = happyShift action_8
action_53 (32) = happyShift action_9
action_53 (33) = happyShift action_10
action_53 (36) = happyShift action_11
action_53 (41) = happyShift action_12
action_53 (42) = happyShift action_13
action_53 (7) = happyGoto action_58
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (38) = happyShift action_57
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (42) = happyShift action_56
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (38) = happyShift action_71
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (39) = happyShift action_69
action_57 (40) = happyShift action_70
action_57 (6) = happyGoto action_68
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (11) = happyShift action_27
action_58 (12) = happyShift action_28
action_58 (13) = happyShift action_29
action_58 (15) = happyShift action_30
action_58 (16) = happyShift action_31
action_58 (17) = happyShift action_32
action_58 (19) = happyShift action_33
action_58 (20) = happyShift action_34
action_58 (21) = happyShift action_35
action_58 (22) = happyShift action_36
action_58 (30) = happyShift action_67
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (11) = happyShift action_27
action_59 (12) = happyShift action_28
action_59 (13) = happyShift action_29
action_59 (15) = happyShift action_30
action_59 (16) = happyShift action_31
action_59 (17) = happyShift action_32
action_59 (19) = happyShift action_33
action_59 (20) = happyShift action_34
action_59 (21) = happyShift action_35
action_59 (22) = happyShift action_36
action_59 _ = happyReduce_2

action_60 (11) = happyShift action_27
action_60 (12) = happyShift action_28
action_60 (13) = happyShift action_29
action_60 (15) = happyShift action_30
action_60 (16) = happyShift action_31
action_60 (17) = happyShift action_32
action_60 (19) = happyShift action_33
action_60 (20) = happyShift action_34
action_60 (21) = happyShift action_35
action_60 (22) = happyShift action_36
action_60 (25) = happyShift action_66
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (38) = happyShift action_65
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (42) = happyShift action_64
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_19

action_64 (38) = happyShift action_77
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (39) = happyShift action_69
action_65 (40) = happyShift action_70
action_65 (6) = happyGoto action_76
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (9) = happyShift action_3
action_66 (14) = happyShift action_4
action_66 (18) = happyShift action_5
action_66 (23) = happyShift action_6
action_66 (27) = happyShift action_7
action_66 (31) = happyShift action_8
action_66 (32) = happyShift action_9
action_66 (33) = happyShift action_10
action_66 (36) = happyShift action_11
action_66 (41) = happyShift action_12
action_66 (42) = happyShift action_13
action_66 (7) = happyGoto action_75
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_24

action_68 (10) = happyShift action_73
action_68 (35) = happyShift action_74
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_3

action_70 _ = happyReduce_4

action_71 (39) = happyShift action_69
action_71 (40) = happyShift action_70
action_71 (6) = happyGoto action_72
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (10) = happyShift action_83
action_72 (35) = happyShift action_74
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (34) = happyShift action_82
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (39) = happyShift action_69
action_74 (40) = happyShift action_70
action_74 (6) = happyGoto action_81
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (11) = happyShift action_27
action_75 (12) = happyShift action_28
action_75 (13) = happyShift action_29
action_75 (15) = happyShift action_30
action_75 (16) = happyShift action_31
action_75 (17) = happyShift action_32
action_75 (19) = happyShift action_33
action_75 (20) = happyShift action_34
action_75 (21) = happyShift action_35
action_75 (22) = happyShift action_36
action_75 (26) = happyShift action_80
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (10) = happyShift action_79
action_76 (35) = happyShift action_74
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (39) = happyShift action_69
action_77 (40) = happyShift action_70
action_77 (6) = happyGoto action_78
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (10) = happyShift action_87
action_78 (35) = happyShift action_74
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (34) = happyShift action_86
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_25

action_81 (35) = happyShift action_74
action_81 _ = happyReduce_5

action_82 (9) = happyShift action_3
action_82 (14) = happyShift action_4
action_82 (18) = happyShift action_5
action_82 (23) = happyShift action_6
action_82 (27) = happyShift action_7
action_82 (31) = happyShift action_8
action_82 (32) = happyShift action_9
action_82 (33) = happyShift action_10
action_82 (36) = happyShift action_11
action_82 (41) = happyShift action_12
action_82 (42) = happyShift action_13
action_82 (7) = happyGoto action_85
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (38) = happyShift action_84
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (39) = happyShift action_69
action_84 (40) = happyShift action_70
action_84 (6) = happyGoto action_91
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (11) = happyShift action_27
action_85 (12) = happyShift action_28
action_85 (13) = happyShift action_29
action_85 (15) = happyShift action_30
action_85 (16) = happyShift action_31
action_85 (17) = happyShift action_32
action_85 (19) = happyShift action_33
action_85 (20) = happyShift action_34
action_85 (21) = happyShift action_35
action_85 (22) = happyShift action_36
action_85 (37) = happyShift action_90
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (9) = happyShift action_3
action_86 (14) = happyShift action_4
action_86 (18) = happyShift action_5
action_86 (23) = happyShift action_6
action_86 (27) = happyShift action_7
action_86 (31) = happyShift action_8
action_86 (32) = happyShift action_9
action_86 (33) = happyShift action_10
action_86 (36) = happyShift action_11
action_86 (41) = happyShift action_12
action_86 (42) = happyShift action_13
action_86 (7) = happyGoto action_89
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (38) = happyShift action_88
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (39) = happyShift action_69
action_88 (40) = happyShift action_70
action_88 (6) = happyGoto action_94
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (11) = happyShift action_27
action_89 (12) = happyShift action_28
action_89 (13) = happyShift action_29
action_89 (15) = happyShift action_30
action_89 (16) = happyShift action_31
action_89 (17) = happyShift action_32
action_89 (19) = happyShift action_33
action_89 (20) = happyShift action_34
action_89 (21) = happyShift action_35
action_89 (22) = happyShift action_36
action_89 (37) = happyShift action_93
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_20

action_91 (34) = happyShift action_92
action_91 (35) = happyShift action_74
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (9) = happyShift action_3
action_92 (14) = happyShift action_4
action_92 (18) = happyShift action_5
action_92 (23) = happyShift action_6
action_92 (27) = happyShift action_7
action_92 (31) = happyShift action_8
action_92 (32) = happyShift action_9
action_92 (33) = happyShift action_10
action_92 (36) = happyShift action_11
action_92 (41) = happyShift action_12
action_92 (42) = happyShift action_13
action_92 (7) = happyGoto action_97
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (9) = happyShift action_3
action_93 (14) = happyShift action_4
action_93 (18) = happyShift action_5
action_93 (23) = happyShift action_6
action_93 (27) = happyShift action_7
action_93 (31) = happyShift action_8
action_93 (32) = happyShift action_9
action_93 (33) = happyShift action_10
action_93 (36) = happyShift action_11
action_93 (41) = happyShift action_12
action_93 (42) = happyShift action_13
action_93 (7) = happyGoto action_96
action_93 _ = happyReduce_20

action_94 (34) = happyShift action_95
action_94 (35) = happyShift action_74
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (9) = happyShift action_3
action_95 (14) = happyShift action_4
action_95 (18) = happyShift action_5
action_95 (23) = happyShift action_6
action_95 (27) = happyShift action_7
action_95 (31) = happyShift action_8
action_95 (32) = happyShift action_9
action_95 (33) = happyShift action_10
action_95 (36) = happyShift action_11
action_95 (41) = happyShift action_12
action_95 (42) = happyShift action_13
action_95 (7) = happyGoto action_100
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (10) = happyShift action_99
action_96 (11) = happyShift action_27
action_96 (12) = happyShift action_28
action_96 (13) = happyShift action_29
action_96 (15) = happyShift action_30
action_96 (16) = happyShift action_31
action_96 (17) = happyShift action_32
action_96 (19) = happyShift action_33
action_96 (20) = happyShift action_34
action_96 (21) = happyShift action_35
action_96 (22) = happyShift action_36
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (11) = happyShift action_27
action_97 (12) = happyShift action_28
action_97 (13) = happyShift action_29
action_97 (15) = happyShift action_30
action_97 (16) = happyShift action_31
action_97 (17) = happyShift action_32
action_97 (19) = happyShift action_33
action_97 (20) = happyShift action_34
action_97 (21) = happyShift action_35
action_97 (22) = happyShift action_36
action_97 (37) = happyShift action_98
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_22

action_99 _ = happyReduce_21

action_100 (11) = happyShift action_27
action_100 (12) = happyShift action_28
action_100 (13) = happyShift action_29
action_100 (15) = happyShift action_30
action_100 (16) = happyShift action_31
action_100 (17) = happyShift action_32
action_100 (19) = happyShift action_33
action_100 (20) = happyShift action_34
action_100 (21) = happyShift action_35
action_100 (22) = happyShift action_36
action_100 (37) = happyShift action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (9) = happyShift action_3
action_101 (14) = happyShift action_4
action_101 (18) = happyShift action_5
action_101 (23) = happyShift action_6
action_101 (27) = happyShift action_7
action_101 (31) = happyShift action_8
action_101 (32) = happyShift action_9
action_101 (33) = happyShift action_10
action_101 (36) = happyShift action_11
action_101 (41) = happyShift action_12
action_101 (42) = happyShift action_13
action_101 (7) = happyGoto action_102
action_101 _ = happyReduce_22

action_102 (10) = happyShift action_103
action_102 (11) = happyShift action_27
action_102 (12) = happyShift action_28
action_102 (13) = happyShift action_29
action_102 (15) = happyShift action_30
action_102 (16) = happyShift action_31
action_102 (17) = happyShift action_32
action_102 (19) = happyShift action_33
action_102 (20) = happyShift action_34
action_102 (21) = happyShift action_35
action_102 (22) = happyShift action_36
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_23

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn5
		 (Decl happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn6
		 (IntType
	)

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn6
		 (BoolType
	)

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (CurryExpr happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (UnExpr Negate happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr Plus happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr Minus happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr Times happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr Equals happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr LessThan happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr GreaterThan happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (UnExpr Not happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr And happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr Or happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  7 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr Xor happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  7 happyReduction_18
happyReduction_18 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (BinExpr Implies happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 7 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (FunAppExpr happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 9 7 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (LambdaExpr happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 12 7 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (LambdaFunAppExpr happy_var_4 happy_var_6 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 12 7 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (NamedFunExpr happy_var_2 happy_var_4 happy_var_6 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 15 7 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_14) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (NamedFunAppExpr happy_var_3 happy_var_5 happy_var_7 happy_var_10 happy_var_12 happy_var_14
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 5 7 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Let happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 7 7 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Ite happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  7 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn7
		 (BoolConst False
	)

happyReduce_27 = happySpecReduce_1  7 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn7
		 (BoolConst True
	)

happyReduce_28 = happySpecReduce_1  7 happyReduction_28
happyReduction_28 (HappyTerminal (CONST happy_var_1))
	 =  HappyAbsSyn7
		 (IntConst happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  7 happyReduction_29
happyReduction_29 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn7
		 (VarExpr happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 43 43 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	EOF happy_dollar_dollar -> cont 8;
	LPAREN happy_dollar_dollar -> cont 9;
	RPAREN happy_dollar_dollar -> cont 10;
	PLUS happy_dollar_dollar -> cont 11;
	MINUS happy_dollar_dollar -> cont 12;
	TIMES happy_dollar_dollar -> cont 13;
	NEGATE happy_dollar_dollar -> cont 14;
	EQUALS happy_dollar_dollar -> cont 15;
	LESSTHAN happy_dollar_dollar -> cont 16;
	GREATERTHAN happy_dollar_dollar -> cont 17;
	NOT happy_dollar_dollar -> cont 18;
	AND happy_dollar_dollar -> cont 19;
	OR happy_dollar_dollar -> cont 20;
	XOR happy_dollar_dollar -> cont 21;
	IMPLIES happy_dollar_dollar -> cont 22;
	IF happy_dollar_dollar -> cont 23;
	THEN happy_dollar_dollar -> cont 24;
	ELSE happy_dollar_dollar -> cont 25;
	FI happy_dollar_dollar -> cont 26;
	LET happy_dollar_dollar -> cont 27;
	ASSIGN happy_dollar_dollar -> cont 28;
	IN happy_dollar_dollar -> cont 29;
	END happy_dollar_dollar -> cont 30;
	FALSE happy_dollar_dollar -> cont 31;
	TRUE happy_dollar_dollar -> cont 32;
	LAMBDA happy_dollar_dollar -> cont 33;
	MAP happy_dollar_dollar -> cont 34;
	ARROW happy_dollar_dollar -> cont 35;
	DEF happy_dollar_dollar -> cont 36;
	DONE happy_dollar_dollar -> cont 37;
	OFTYPE happy_dollar_dollar -> cont 38;
	Int_Type happy_dollar_dollar -> cont 39;
	Bool_Type happy_dollar_dollar -> cont 40;
	CONST happy_dollar_dollar -> cont 41;
	ID happy_dollar_dollar -> cont 42;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 43 tk tks = happyError' (tks, explist)
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
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError [] = error $ "Syntax Error in the last token"
parseError (x:xs) = error $ "Syntax Error: " ++ (show x)
parse p = parser (alexScanTokens p)
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
