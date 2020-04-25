--Exercise 2--
--2.a--
data Circuit = A Gates Links
data Gates = B (Int,GateFn) Gates | Empty
data GateFn = And
	     | Or
	     | Xor
	     | Not
data Links = From (Int,Int) (Int,Int) Links | Empty

--2.b--
halfAdder = A (B (1,Xor) (B (2,And)) Empty) (From (1,1) (2,1) (From (1,2) (2,2) Empty))

--2.c--
ppCircuit :: Circuit -> String
ppCircuit A G L = ppGates G ++ ppLinks L

ppGates :: Gates -> String
ppGates Empty = ""
ppGates B (int1,str) next = int1 ++ ":" ++ ppGateFn str ++ ";" ++ ppGates next

ppGateFn :: GateFn -> String
ppGateFn And = "and"
ppGateFn Or = "or"
ppGateFn Xor = "xor"
ppGateFn Not = "not"  	

ppLinks :: Links -> String
ppLinks Empty = ""
ppLinks From (int1,int2) (int3,int4) next = "from " ++ int1 ++ "." ++ int2 ++ " to " ++ int3 ++ "." ++ int4 ++ ";" ++ ppLinks next 	 
