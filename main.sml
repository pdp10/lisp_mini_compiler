
(* MAIN *)

use "interpreter.sml";



(*
 ************************************************************************************* 
 * SOME LISP PROGRAMS (in simple form, after syntax analysis) 
 *************************************************************************************
 *)



print("\n\n\nHere, there are some examples of compilation from Lisp\n" ^
      "language (SF) to SECD language. After the SECD code, there\n" ^ 
      "is the interpretation of the abstract SECD machine.\n\n\n");



(* 
 * Let( N =3 in LAMBDA(P,Q,R) (DIV (ADD (MUL P,P),(ADD(MUL Q,Q),(MUL R,R))),N)(X,Y,Z)
 * Environment (X=3,Y=6,Z=9)
 * Result: val q1 = V(NUM 42) 
 *)
val a1 = startCOMP(Let(Call(Lambda(["P","Q","R"],Op("DIV",[Op("ADD",[Op("MUL",[Var "P", Var "P"]),Op("ADD",[Op("MUL",[Var "Q", Var "Q"]),Op("MUL",[Var "R", Var "R"])])]), Var "N"])),[Var "X", Var "Y", Var "Z"]),["N"],[Quote (NUM 3)]), [["X","Y","Z"]], [STOP]);
val q1 = lancio([],[[V(NUM 3),V(NUM 6),V(NUM 9)]], a1, []);






(* 
 * Let N=2 in if (LEQ(X,Y)) then (LAMBDA(P,Q) (DIV (SUB P, Q ),N)(Y,X)) else (LAMBDA(P,Q) (DIV (SUB P,Q),N)(X,Y))
 * Environment (T=3, S=4, X=20, Y=10)
 * Result val q1 = V(NUM 5) 
 *)
val a2 = startCOMP(Let(If(Op("LEQ", [Var "X", Var "Y"]),
Call(Lambda(["P","Q"],Op("DIV",[Op("SUB",[Var "P", Var "Q"]),Var "N"])), [Var "Y", Var "X"]),
Call(Lambda(["P","Q"],Op("DIV",[Op("SUB",[Var "P", Var "Q"]),Var "N"])), [Var "X", Var "Y"])),
["N"],[Quote (NUM 2)]), 
[["T","S"],["X","Y"]],[STOP]);
val q2 = lancio([], [[V(NUM 3),V(NUM 4)],[V(NUM 20),V(NUM 10)]], a2, []);






(* 
 * Let N=5 in (CONS (ADD X,N), (CONS (ADD Y,N), (CONS (ADD Z, N), NIL)))
 * Environment (T=8,S=2,X=3,Y=6,z=9)
 * Result val q3 = VLISTA [V(NUM 8), V(NUM 11), V(NUM 14), V NIL] 
 *)
val a3 = startCOMP(Let( 
Op("CONS",[Op("ADD",[Var "X", Var "N"]),
Op("CONS",[Op("ADD",[Var "Y", Var "N"]), 
Op("CONS",[Op("ADD",[Var "Z", Var"N"]),Quote (NIL)])])]),
["N"],[Quote (NUM 5)]), 
[["T","S"],["X","Y", "Z"]],[STOP]);
val q3 = lancio([], [[V(NUM 8), V(NUM 2)],[V(NUM 3),V(NUM 6),V(NUM 9)]], a3, []);





(*
 * let f= lambda(x).5+x; a=lambda(k,m).k(m); in a(f,3) end;
 * result: 3+5=8
 *)
val a4 = startCOMP(Let(Call(Var "A",[Var "F",Quote (NUM 3)]),["F","A"],[Lambda(["X"],Op("ADD",[Quote(NUM 5),Var "X"])),Lambda(["K","M"], Call(Var "K",[Var "M"]))]),[],[STOP]);
val q4= lancio([], [], a4, []);







(*
 * let f=lambda(x). x+5; d=lambda(k).lambda(y).k(k(y)) in (d(f))(3) end;
 * result 3+5+5=13
 *)
val a5 = startCOMP(Let(Call(Call(Var "D",[Var "F"]),[Quote (NUM 3)]),["F","D"],
[Lambda(["X"],Op("ADD",[Quote(NUM 5),Var "X"])),
Lambda(["K"], Lambda(["Y"],Call(Var "K",[Call(Var "K",[Var "Y"])])))]), [], [STOP]);
val q5= lancio([], [], a5,[]);







(* 
 * (lambda(x)(x+y)) 
 * result: CLO([LD(0, 0), LD(1, 1), ADD, RTN], [])
 *)
val b1 = startCOMP(Lambda(["X"], Op("ADD", [Var "X",Var "Y"])), [["X","Y"]], [STOP]); 
val r1 = lancio([],[],b1,[]); 






(* 
 * (x/y) 
 * result: V(NUM 3)
 *)
val b2 = startCOMP(Op("DIV",[Var "X",Var "Y"]),[["X","Y"]],[STOP]);
val r2 = lancio([],[[V(NUM 27), V(NUM 9)]], b2, []);







(* 
 * (car(R S T U V))
 * result: V(NUM 5)
 *)

val b3 = startCOMP(
Op("CAR", [Op("CONS", [Var "R", Op("CONS", [Var "S", Op("CONS", [Var "T", Op("CONS", [Var "U", Op("CONS", [Var "V", Quote(NIL)])])])])])]),
[["R","S","T","U","V"]],
[STOP]);
val r3 = lancio([], [[V(NUM 5),V(NUM 9),V(NUM 2),V(NUM 6),V(NUM 8)]], b3, []);






(* 
 * (cdr(R S T U V))
 * result: VLISTA [V(NUM 9), V(NUM 2), V(NUM 6), V(NUM 8)]
 *)
val b4 = startCOMP(
Op("CDR", [Op("CONS", [Var "R", Op("CONS", [Var "S", Op("CONS", [Var "T", Op("CONS", [Var "U", Op("CONS", [Var "V", Quote(NIL)])])])])])]),
[["R","S","T","U","V"]],
[STOP]);
val r4 = lancio([],[[V(NUM 5),V(NUM 9),V(NUM 2),V(NUM 6),V(NUM 8)]],b4,[]);






(* 
 * atom(3) 
 * result: V T
 *)
val b5 = startCOMP(Op("ATOM",[Quote(NUM(3))]),[["X"]],[STOP]);
val r5 = lancio([],[],b5,[]);






(* 
 * (cond((3=5) "THEN") (T "ELSE")
 * result: V(STRINGA "ELSE")
 *)
val b6 = startCOMP(If(Op("EQ", [Quote(NUM 3), Quote(NUM 5)]), Quote(STRINGA "THEN"), Quote(STRINGA "ELSE")), [], [STOP]);
val r6= lancio([],[],b6,[]);





(* 
 * (cond((3=5) (lambda(x, y)(x+y))(2, z)) (T (lambda(x, y)(x-y))(5, z)))  
 * result: V(NUM ~10)
 *) 
val b7 = startCOMP(If(Op("EQ", [Quote(NUM 3), Quote(NUM 5)]), 
Call(Lambda(["X", "Y"], Op("ADD", [Var "X",Var "Y"])), [Quote(NUM(2)), Var "Z"]),
Call(Lambda(["X", "Y"], Op("SUB", [Var "X",Var "Y"])), [Quote(NUM(5)), Var "Z"])),
[["Z"],["X","Y"]], [STOP]);
val r7= lancio([],[[V(NUM 15)]],b7,[]);







(* 
 * (lambda xy.(x*y)(3 5)
 * result: V(NUM 15)
 *)
val b8 = startCOMP(Call(Lambda(["X","Y"], Op("MUL", [Var "X",Var "Y"])), [Quote(NUM(3)), Quote(NUM(5))]),[["X","Y"]],[STOP]);
val r8= lancio([],[],b8,[]);
















(* Recursive examples *)

(*
   (1) Factorial 

   Letrec 
     FACT = Lambda(X) If (X=0) then 1 else X*FACT(X-1)
   in
     FACT(3)
   end;
  
   Result:

  [DUM, LDC NIL,
     LDF [LD(0, 0), LDC(NUM 0), EQ,
          SEL([LDC(NUM 1), JOIN],
              [LD(0, 0), LDC NIL, LD(0, 0), LDC(NUM 1), SUB, CONS, LD(1, 0),
               AP, MUL, JOIN]), RTN], CONS,
     LDF [LDC NIL, LDC(NUM 3), CONS, LD(0, 0), AP, RTN], RAP]


 *)

val lr1 = 
startCOMP( Letrec(Call( Var "FACT", [Quote (NUM 3)]),
       ["FACT"],
       [Lambda(["X"], If(Op( "EQ", [Var "X", Quote (NUM 0)]), Quote
       (NUM 1), 
       Op("MUL", [Var "X",Call( Var "FACT", [Op("SUB",[Var "X", Quote (NUM
       1)])])] ))) ]), [], []); 

val lri1 = 
lancio([],[],
	startCOMP( Letrec(Call( Var "FACT", [Quote (NUM 3)]),
       ["FACT"],
       [Lambda(["X"], If(Op( "EQ", [Var "X", Quote (NUM 0)]), Quote
       (NUM 1), 
       Op("MUL", [Var "X",Call( Var "FACT", [Op("SUB",[Var "X", Quote (NUM
       1)])])] ))) ]), [], [STOP]),
[]);










(* (2) list factorial
  
 Letrec
	FACT = Lambda(X) If (X=0) then 1 else X*FACT(X-1)
        F= Lambda(G,L) If (L=NIL) then L else
                          CONS(G(CAR(L)),F(G,TL(L)))
    in
       F(FACT,[3,4,5])
                              
*)


val lr2 = 
startCOMP(Letrec(
        Call( Var "F",[ Var "FACT", Quote(DOT( NUM 3, DOT( NUM 4, DOT(NUM
        5, NIL))))]),
        ["FACT","F"],
        [Lambda(["X"], If(Op( "EQ", [Var "X", Quote (NUM 0)]), Quote
       (NUM 1), 
       Op("MUL", [Var "X",Call( Var "FACT", [Op("SUB",[Var "X", Quote (NUM
       1)])])] ))),
         Lambda(["G","L"], If( Op("EQ", [Var "L", Quote(NIL)]),
         Var "L",
         Op("CONS",[Call(Var "G",[Op("CAR",[Var "L"])]), 
                    Call(Var "F",[Var "G", Op("CDR",[Var "L"])])])))]),[],[]);

val lri2 = 
lancio([], [], 
	startCOMP(Letrec(
        Call( Var "F",[ Var "FACT", Quote(DOT( NUM 3, DOT( NUM 4, DOT(NUM
        5, NIL))))]),
        ["FACT","F"],
        [Lambda(["X"], If(Op( "EQ", [Var "X", Quote (NUM 0)]), Quote
       (NUM 1), 
       Op("MUL", [Var "X",Call( Var "FACT", [Op("SUB",[Var "X", Quote (NUM
       1)])])] ))),
         Lambda(["G","L"], If( Op("EQ", [Var "L", Quote(NIL)]),
         Var "L",
         Op("CONS",[Call(Var "G",[Op("CAR",[Var "L"])]), 
                    Call(Var "F",[Var "G", Op("CDR",[Var "L"])])])))]),[],[STOP]),
[]);






(* 
 (3) append function of two lists

 Letrec  
     Append = Lambda(X,Y) If (X = NIL) then Y else 
                              CONS(CAR(X),Append(CDR(X),Y))
     in 
       Append([1,2],[3,4])
     end;  

 Result:

[DUM, LDC NIL,
     LDF [LD(0, 0), LDC NIL, EQ,
          SEL([LD(0, 1), JOIN],
              [LDC NIL, LD(0, 1), CONS, LD(0, 0), CDR, CONS, LD(1, 0), AP,
               LD(0, 0), CAR, CONS, JOIN]), RTN], CONS,
     LDF [LDC NIL, LDC(DOT(NUM 3, DOT(NUM 4, NIL))), CONS,
          LDC(DOT(NUM 1, DOT(NUM 2, NIL))), CONS, LD(0, 0), AP, RTN], RAP]

*)



val lr3 = startCOMP(Letrec(Call( Var "Append", [Quote (DOT(NUM 1, DOT(NUM 2, NIL))), 
                             Quote (DOT(NUM 3, DOT(NUM 4, NIL)))]),["Append"],
              [Lambda(["X","Y"], If(Op ("EQ", [Var "X",Quote(NIL)]),Var "Y", 
	      Op( "CONS", [Op("CAR", [Var"X"]),
                             Call(Var "Append",[Op("CDR",[Var "X"]), Var "Y"])])))]), [],[]);


val lri3 = 
lancio([],[],
startCOMP(Letrec(Call( Var "Append", [Quote (DOT(NUM 1, DOT(NUM 2, NIL))), 
                             Quote (DOT(NUM 3, DOT(NUM 4, NIL)))]),["Append"],
              [Lambda(["X","Y"], If(Op ("EQ", [Var "X",Quote(NIL)]),Var "Y", 
	      Op( "CONS", [Op("CAR", [Var"X"]),
                             Call(Var "Append",[Op("CDR",[Var "X"]), Var "Y"])])))]), [],[STOP]),
[]);