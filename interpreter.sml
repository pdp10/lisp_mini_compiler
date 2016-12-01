

use "compiler.sml";




(******************************** THE SECD INTERPRETER *****************************)




datatype Valore = V of s_espressione | 
                  CLO of secdexpr list * (unit -> Valore list list)|
                  VLISTA of Valore list |
		  OMEGA (* Attenzione: nuovo valore *)

exception Error of string;
exception VuotaHd;
exception VuotaTl;

(* funzioni hd e tl con eccezioni ad hoc *)

fun HA(a:'a list):'a =
case a of 
[]=> raise VuotaHd |
x::y => x;

fun TA(a:'a list):'a list = 
case a of
[]=> raise VuotaTl | 
x::y => y;

(* la funzione RecApp (assieme a In) costruisce l'ambiente ricorsivo *)

fun  RecApp(LP: Valore list, a: Valore list list):Valore list list= 
In(LP,a,LP)::TA(a) (* TA(a) toglie l'Omega dalla cima di E *)
and 
In(R:Valore list, a:Valore list list, A:Valore list):Valore list= 
(case R of
[] => [] |
CLO(x,b)::y => 
(case HA(HA(b()))handle VuotoHd =>(print("RecApp");V(NIL))of 
       OMEGA=> CLO(x,fn()=> RecApp(A,a))::In(y,a,A) |
        Z=> raise Error("ambiente senza omega in In dentro a RAP")) |
Z=> raise Error("errore in RAP: trovato parametro non chiusura"));

datatype Dump = CONTR of secdexpr list | 
                TRIPLA of Valore list * Valore list list * secdexpr list |
		DUMMY


fun index (n: int, s: 'a list): 'a=
           if n=0 then HA(s) else index(n-1,TA(s));  


fun locate(i: int * int, e: Valore list list):Valore=
       let 
          val b = #1(i);
          val n = #2(i);
       in 
         index(n,index(b,e))
       end;




val extract_int   = fn V(NUM(x)) => x |
                    Q => raise Error("trovato altro da intero");





val Vhd = fn VLISTA(a::b) => a |
          Q  => raise Error("non lista");

fun Vtl(VLISTA(a::[])) = VLISTA([]) |
    Vtl(VLISTA(a::b)) = VLISTA(b)  |
    Vtl(Q)  = raise Error("non lista");


fun Vatom(a : Valore):Valore=
         case a of 
              V(K) =>
                   (case K of 
                         DOT(s1,s2) => V(F)|
                         Q2 => V(T) )|
              Q1 => V(F);         
                                        
fun bool2s_espressione(b: bool): s_espressione=
         if b then T else F;


fun build_list(d: s_espressione): Valore list =
     (case d of  
            DOT(a,b) => (case a of 
		     DOT(c,d) => VLISTA(build_list(a))::build_list(b)
		     |
		     NIL => VLISTA([])::build_list(b)
		     |
		     X => V(a)::build_list(b))
	    |
	    Nil => []);



fun interprete (S: Valore list, E: Valore list list, C: secdexpr list, D: Dump list): Valore=  
case HA(C)handle VuotoHd =>(print("TOP");STOP) of           
           LD(b,n) => 
              let 
                val x = locate((b,n),E) handle VuotoHd => raise Error("in locate");
              in 
                interprete(x::S,E,TA(C),D)
              end |           
	      LDC(k) => 
		     
		     (case k of 
		     NIL => interprete(VLISTA([])::S,E,TA(C),D)|
		     DOT(a,b) => interprete(VLISTA(build_list(k))::S,E,TA(C),D) |
		     x => interprete(V(k)::S,E,TA(C),D))
                 |
              ADD =>
              let 
                val operand1 = extract_int(HA(S));
                val operand2 = extract_int(HA(TA(S)));
              in  
                interprete(V(NUM (operand1 + operand2))::TA(TA(S)),E,TA(C),D)
             end |
             
            SUB =>
              let 
                val operand1 = extract_int(HA(S)handle VuotoHd =>(print("SUB");V(NIL)));
                val operand2 = extract_int(HA(TA(S))handle VuotoHd =>(print("SUB1");V(NIL)));
              in  
                interprete(V(NUM (operand2 - operand1))::TA(TA(S)),E,TA(C),D)
             end |
            
            MUL =>
              let 
                val operand1 = extract_int(HA(S)handle VuotoHd =>(print("MUL");V(NIL)));
                val operand2 = extract_int(HA(TA(S))handle VuotoHd =>(print("MUL1");V(NIL)));
              in  
                interprete(V(NUM (operand1*operand2))::TA(TA(S)),E,TA(C),D)
             end |
            
             DIV =>
              let 
                val operand1 = extract_int(HA(S));
                val operand2 = extract_int(HA(TA(S)));
              in  
                interprete(V(NUM (operand2 div operand1))::TA(TA(S)),E,TA(C),D)
             end |
          
	    REM =>
              let 
                val operand1 = extract_int(HA(S));
                val operand2 = extract_int(HA(TA(S)));
              in  
                interprete(V(NUM (operand2 mod operand1))::TA(TA(S)),E,TA(C),D)
             end |
             
             LEQ =>
              let 
                val operand1 = extract_int(HA(S));
                val operand2 = extract_int(HA(TA(S)));
              in  
                interprete(V(bool2s_espressione(operand2 <= operand1))::TA(TA(S)),E,TA(C),D)
             end |
           
              EQ =>
		 let val x=(HA(S) handle VuotoHd => (print("1");V(NIL))) and 
		 y=(HA(TA(S))handle VuotoHd => (print("2");V(NIL)) )in
	         (case x of
		       V(a)=> (case y of 
			      V(b) => interprete(V(bool2s_espressione(a=b))::TA(TA(S)),E,TA(C),D) |
			      X => interprete(V(F)::TA(TA(S)),E,TA(C),D) )
		       |
		       VLISTA([]) => (case y of 
				 VLISTA([]) => interprete(V(T)::TA(TA(S)),E,TA(C),D)|
				 X => interprete(V(F)::TA(TA(S)),E,TA(C),D))
		       |
		       X => raise Error("Valore non confrontabile in EQ"))
		       end 
		       |
              CAR => 
                 interprete(Vhd(HA(S)handle VuotoHd =>(print("CAR");V(NIL)))::TA(S),E,TA(C),D) 
		 |
 
              CDR => 
                 interprete(Vtl(HA(S)handle VuotoHd =>(print("CDR");V(NIL)))::TA(S),E,TA(C),D)
		 |
              
              CONS => 
                 (case HA(TA(S))handle VuotoHd =>(print("CONS");V(NIL)) of 
                      VLISTA([])=>
                         interprete(VLISTA([HA(S)handle VuotoHd =>(print("CONS2");V(NIL))])::TA(TA(S)),E,TA(C),D) | 
                      
                      VLISTA(vl2) =>
                         interprete(VLISTA((HA(S)handle VuotoHd =>(print("CONS3");V(NIL)))::vl2)::TA(TA(S)),E,TA(C),D)| 
		      X => 
		         raise Error("CONS: il secondo argomento non e' una lista"))
                     |                                   
                    
                 ATOM => 
                 interprete(Vatom(HA(S))::TA(S),E,TA(C),D)|
            
              SEL(sl1,sl2) =>
                 
                   (case HA(S)handle VuotoHd =>(print("SEL");V(NIL)) of
                         V(T) => interprete(TA(S), E,sl1@TA(C),CONTR(TA(C))::D)|
                         V(F) => interprete(TA(S), E,sl2@TA(C),CONTR(TA(C))::D)|
                         Q =>  raise Error("SEL: non trovato bool su S"))
                 | 
               
              JOIN => 
                   (case HA(D)handle VuotoHd =>(print("JOIN");DUMMY) of 
                             CONTR(C1) => interprete(S,E,C1,TA(D))| 
                             X=> raise 
			     Error("JOIN: il dump non contiene controllo") )
                   |

              LDF(sl) =>
                      interprete(CLO(sl,fn()=>E)::S,E,TA(C),D)|
              
              AP =>
                    (case HA(S) handle VuotoHd =>(print("AP");V(NIL)) of
		     CLO(c1,e1)=> 
		        (case HA(TA(S))handle VuotoHd =>(print("AP2");V(NIL)) of
			       VLISTA([]) => interprete([],[]::e1(), c1,TRIPLA(TA(TA(S)),E,TA(C))::D)| 
			       VLISTA(vl2) => interprete([],vl2::e1(), c1,TRIPLA(TA(TA(S)),E,TA(C))::D)| 
			       X=> raise Error("AP: non ci sono i parametri attuali"))|
		      X => raise Error("AP: non trovata chiusura su S"))	                      
                 | 

                 RTN => 
                        (case HA(D)handle VuotoHd =>(print("RTN");DUMMY) of 
			TRIPLA(s1,e1,c1)=>    interprete(HA(S)::s1,e1,c1,TA(D))|
			X =>  raise Error("RTN: non trovata TRIPLA su dump"))   
                               
                    | 
		 DUM =>
			interprete(S,[OMEGA]::E,TA(C),D) 
		    |
		RAP =>
			(case HA(S)handle VuotoHd =>(print("RAP");V(NIL)) of
			CLO(c1,e1)=> 
				     (case HA(TA(S))handle VuotoHd =>(print("RAP2");V(NIL)) of
				     VLISTA([]) => interprete([],[]::e1(), c1,TRIPLA(TA(TA(S)),TA(E),TA(C))::D)| 
				     VLISTA(vl2) => interprete([],RecApp(vl2,e1()) handle VuotoHd => 
				     (print("in Rap");[]), c1, TRIPLA(TA(TA(S)),TA(E),TA(C))::D)| 
				     X=> raise Error("RAP: non ci sono i parametri attuali"))|
		        X => raise Error("AP: non trovata chiusura su S"))	                      
                 | 
                STOP => HA(S)handle VuotoHd =>(print("STOP");V(NIL)); 



(* lancio serve per lanciare l'interprete e gestire le sue  eccezioni *)
fun lancio(x,y,z,w)=
interprete(x,y,z,w) handle Error(a) =>  (print(a);V(NUM (~1)))|
		    VuotoHd => V(NUM(~2));