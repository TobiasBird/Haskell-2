

--Exercise 1--
--1.a--
data Mode = Up | Down deriving(Show, Eq)
data Pos = POS1 Int | POS2 String deriving(Show, Eq)
data Pars = PARS1 String Pars | PARS2 String deriving(Show, Eq)
data Vals = VALS1 Int Vals | VALS2 Int deriving(Show, Eq)

data Cmd = Pen Mode | Moveto (Pos, Pos) | Def String Pars Cmd | Call String Vals | CMD1 Cmd Cmd deriving(Show, Eq)

--1.b--
vector = Def "vector" (PARS1 "x1" (PARS1 "y1" (PARS1 "x2" (PARS2 "y2")))) (CMD1 (CMD1 (Pen Up) (Moveto (POS2 "x1",POS2 "y1")) ) (CMD1 (Pen Down) (Moveto (POS2 "x2",POS2 "y2"))))

--1.c--
steps :: Int -> Cmd
steps 0 = CMD1 (Moveto (POS1 0, POS1 0)) (Pen Down)
steps num = CMD1  (steps (num-1)) (CMD1 (Moveto (POS1 (num-1), POS1 num)) (Moveto (POS1 (num),POS1 (num))))

--Exercise 2--
--2.a--
data Circuit = A Gates Links
data Gates = B (Int,GateFn) Gates | EmptyG
data GateFn = And | Or | Xor | Not
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
