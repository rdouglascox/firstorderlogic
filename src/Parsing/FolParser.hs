{-# OPTIONS_GHC -w #-}
module Parsing.FolParser where
import Parsing.FolToken
import Data.Prop
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (FolToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,82) ([7232,4110,0,0,3584,0,0,0,0,0,14449,256,16384,0,57796,32768,16391,3612,34576,3,0,14449,0,57344,0,56,0,32800,3,0,0,0,18304,0,0,0,57796,28928,16440,3612,34576,3,256,16384,0,16,1024,0,0,32832,3,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_happyFolParser","form","fol","terms","pred","const","vari","funct","top","bot","\"~\"","\"&\"","\"v\"","\"->\"","\"<->\"","\"@\"","\"#\"","\"(\"","\")\"","%eof"]
        bit_start = st Prelude.* 22
        bit_end = (st Prelude.+ 1) Prelude.* 22
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..21]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (7) = happyShift action_3
action_0 (11) = happyShift action_5
action_0 (12) = happyShift action_6
action_0 (13) = happyShift action_7
action_0 (18) = happyShift action_8
action_0 (19) = happyShift action_9
action_0 (20) = happyShift action_10
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (7) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (8) = happyShift action_17
action_3 (9) = happyShift action_18
action_3 (10) = happyShift action_19
action_3 (6) = happyGoto action_16
action_3 _ = happyReduce_13

action_4 (22) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_3

action_6 _ = happyReduce_2

action_7 (7) = happyShift action_3
action_7 (11) = happyShift action_5
action_7 (12) = happyShift action_6
action_7 (13) = happyShift action_7
action_7 (18) = happyShift action_8
action_7 (19) = happyShift action_9
action_7 (20) = happyShift action_15
action_7 (4) = happyGoto action_14
action_7 (5) = happyGoto action_2
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (9) = happyShift action_13
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (9) = happyShift action_12
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (7) = happyShift action_3
action_10 (11) = happyShift action_5
action_10 (12) = happyShift action_6
action_10 (13) = happyShift action_7
action_10 (18) = happyShift action_8
action_10 (19) = happyShift action_9
action_10 (20) = happyShift action_10
action_10 (4) = happyGoto action_11
action_10 (5) = happyGoto action_2
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (14) = happyShift action_26
action_11 (15) = happyShift action_27
action_11 (16) = happyShift action_28
action_11 (17) = happyShift action_29
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (7) = happyShift action_3
action_12 (11) = happyShift action_5
action_12 (12) = happyShift action_6
action_12 (13) = happyShift action_7
action_12 (18) = happyShift action_8
action_12 (19) = happyShift action_9
action_12 (20) = happyShift action_10
action_12 (4) = happyGoto action_25
action_12 (5) = happyGoto action_2
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (7) = happyShift action_3
action_13 (11) = happyShift action_5
action_13 (12) = happyShift action_6
action_13 (13) = happyShift action_7
action_13 (18) = happyShift action_8
action_13 (19) = happyShift action_9
action_13 (20) = happyShift action_10
action_13 (4) = happyGoto action_24
action_13 (5) = happyGoto action_2
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_4

action_15 (7) = happyShift action_3
action_15 (11) = happyShift action_5
action_15 (12) = happyShift action_6
action_15 (13) = happyShift action_7
action_15 (18) = happyShift action_8
action_15 (19) = happyShift action_9
action_15 (20) = happyShift action_10
action_15 (4) = happyGoto action_23
action_15 (5) = happyGoto action_2
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_12

action_17 (8) = happyShift action_17
action_17 (9) = happyShift action_18
action_17 (10) = happyShift action_19
action_17 (6) = happyGoto action_22
action_17 _ = happyReduce_14

action_18 (8) = happyShift action_17
action_18 (9) = happyShift action_18
action_18 (10) = happyShift action_19
action_18 (6) = happyGoto action_21
action_18 _ = happyReduce_15

action_19 (20) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (8) = happyShift action_17
action_20 (9) = happyShift action_18
action_20 (10) = happyShift action_19
action_20 (6) = happyGoto action_35
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_18

action_22 _ = happyReduce_17

action_23 (14) = happyShift action_26
action_23 (15) = happyShift action_27
action_23 (16) = happyShift action_28
action_23 (17) = happyShift action_29
action_23 (21) = happyShift action_34
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_10

action_25 _ = happyReduce_11

action_26 (7) = happyShift action_3
action_26 (11) = happyShift action_5
action_26 (12) = happyShift action_6
action_26 (13) = happyShift action_7
action_26 (18) = happyShift action_8
action_26 (19) = happyShift action_9
action_26 (20) = happyShift action_10
action_26 (4) = happyGoto action_33
action_26 (5) = happyGoto action_2
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (7) = happyShift action_3
action_27 (11) = happyShift action_5
action_27 (12) = happyShift action_6
action_27 (13) = happyShift action_7
action_27 (18) = happyShift action_8
action_27 (19) = happyShift action_9
action_27 (20) = happyShift action_10
action_27 (4) = happyGoto action_32
action_27 (5) = happyGoto action_2
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (7) = happyShift action_3
action_28 (11) = happyShift action_5
action_28 (12) = happyShift action_6
action_28 (13) = happyShift action_7
action_28 (18) = happyShift action_8
action_28 (19) = happyShift action_9
action_28 (20) = happyShift action_10
action_28 (4) = happyGoto action_31
action_28 (5) = happyGoto action_2
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (7) = happyShift action_3
action_29 (11) = happyShift action_5
action_29 (12) = happyShift action_6
action_29 (13) = happyShift action_7
action_29 (18) = happyShift action_8
action_29 (19) = happyShift action_9
action_29 (20) = happyShift action_10
action_29 (4) = happyGoto action_30
action_29 (5) = happyGoto action_2
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (21) = happyShift action_40
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (21) = happyShift action_39
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (21) = happyShift action_38
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (21) = happyShift action_37
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_5

action_35 (21) = happyShift action_36
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (8) = happyShift action_17
action_36 (9) = happyShift action_18
action_36 (10) = happyShift action_19
action_36 (6) = happyGoto action_41
action_36 _ = happyReduce_16

action_37 _ = happyReduce_6

action_38 _ = happyReduce_7

action_39 _ = happyReduce_8

action_40 _ = happyReduce_9

action_41 _ = happyReduce_19

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Atom happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (Bot
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (Top
	)

happyReduce_4 = happySpecReduce_2  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Not happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Not happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 4 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (And happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 4 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Or happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 5 4 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Imp happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 5 4 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Iff happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal (VariableSymbol happy_var_2))
	_
	 =  HappyAbsSyn4
		 (Forall happy_var_2 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal (VariableSymbol happy_var_2))
	_
	 =  HappyAbsSyn4
		 (Exists happy_var_2 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (PredicateSymbol happy_var_1))
	 =  HappyAbsSyn5
		 (R happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 (HappyTerminal (PredicateSymbol happy_var_1))
	 =  HappyAbsSyn5
		 (R happy_var_1 []
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 (HappyTerminal (ConstantSymbol happy_var_1))
	 =  HappyAbsSyn6
		 ([Fn happy_var_1 []]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  6 happyReduction_15
happyReduction_15 (HappyTerminal (VariableSymbol happy_var_1))
	 =  HappyAbsSyn6
		 ([Var happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 6 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (FunctionSymbol happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ([Fn happy_var_1 happy_var_3]
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (ConstantSymbol happy_var_1))
	 =  HappyAbsSyn6
		 ((Fn happy_var_1 []) : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (VariableSymbol happy_var_1))
	 =  HappyAbsSyn6
		 ((Var happy_var_1) : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 5 6 happyReduction_19
happyReduction_19 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (FunctionSymbol happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((Fn happy_var_1 happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PredicateSymbol happy_dollar_dollar -> cont 7;
	ConstantSymbol happy_dollar_dollar -> cont 8;
	VariableSymbol happy_dollar_dollar -> cont 9;
	FunctionSymbol happy_dollar_dollar -> cont 10;
	TopSymbol -> cont 11;
	BotSymbol -> cont 12;
	NegationSymbol -> cont 13;
	ConjunctionSymbol -> cont 14;
	DisjunctionSymbol -> cont 15;
	ConditionalSymbol -> cont 16;
	BiconditionalSymbol -> cont 17;
	UniversalSymbol -> cont 18;
	ExistentialSymbol -> cont 19;
	LeftPar -> cont 20;
	RightPar -> cont 21;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 22 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => E a -> (a -> E b) -> E b
happyThen = (thenE)
happyReturn :: () => a -> E a
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> E a
happyReturn1 = \a tks -> (returnE) a
happyError' :: () => ([(FolToken)], [Prelude.String]) -> E a
happyError' = (\(tokens, _) -> parseError tokens)
happyFolParser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
   case m of
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
   case m of
      Ok a -> Ok a
      Failed e -> k e

parseError tokens = failE "Parse error"
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
