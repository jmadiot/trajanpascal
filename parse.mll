{
type expression = 
	T of terminal
;;

type nativetype = Integer | Boolean | JillJenn;;

type ttype =
	| Native of nativetype
	| Array of int*int*ttype
	| Record of identificateur*ttype array
;;

type typedeclaration = identificateur * ttype;;

type vardeclaration = identificateur * identificateur;;

type procedure = {
	declaration : declaration;
	corps : statement
}
;;


type statement =
	| While of expression * statement
	| IfThen of expression * statement
	| IfThenElse of expression * statement * statement
	| Affect of reference * expression
	| Pvirgule of statement * statement
;;

rule build q = parse
  | '['	
  	{
  		let b = Boucle (build (Queue.create()) lexbuf) in
  		Queue.push b q;
  		build q lexbuf
  	}
  | ']' { N q }
  | ['<' '>' '+' '-' '.' ',']+ as cmd 
  	{
  		Queue.push (F cmd) q;
  		build q lexbuf
  	} 
  | _
  	{
  		build q lexbuf
  	} 
  | eof
  	{
  		N q
  	}

{
  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    let a = build (Queue.create()) lexbuf in
    (*print_arbre a;*)
	exec_arbre a;;
  let _ = Printexc.print main ()
}
