--Exercise 1--
--1.a--

data Cmd = pen Mode
					| moveto (Pos, Pos)
					| def String ( Pars ) Cmd
					| call String ( Vals )
					| Cmd Cmd

data Mode = Up
					| Down
data Pos = Int
					| String
data Pars = String Pars
					| String
data Vals = Int Vals
					| Int

pen :: Mode -> Bool
pen Mode 	| Mode == Up = False
					| Mode == Down = True

--Exercise 2--
--2.a--
data Circuit = A Gates Links
data Gates = B (Int,GateFn) Gates | EmptyG
data GateFn = And
	     | Or
	     | Xor
	     | Not
data Links = From (Int,Int) (Int,Int) Links | EmptyL

--2.b--
halfAdder = A (B (1,Xor) (B (2,And) EmptyG)) (From (1,1) (2,1) (From (1,2) (2,2) EmptyL))

--2.c--
instance Show Circuit where
	show = ppCircuit

ppCircuit :: Circuit -> String
ppCircuit (A g l) = ppGates g ++ ppLinks l

ppGates :: Gates -> String
ppGates EmptyG = ""
ppGates (B (int1,str) next) = show int1++":"++ppGateFn str++";\n"++ppGates next

ppGateFn :: GateFn -> String
ppGateFn And = "and"
ppGateFn Or = "or"
ppGateFn Xor = "xor"
ppGateFn Not = "not"

ppLinks :: Links -> String
ppLinks EmptyL = ""
ppLinks (From (int1,int2) (int3,int4) next) = "from "++show int1++"."++show int2++" to "++show int3++"."++show int4++";\n"++ppLinks next
