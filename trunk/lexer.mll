{

type basetoken = 
	| BEGIN | END | WHILE | IF | THEN | ELSE | DO
	
	| VAR | TYPE | OF
	| ARRAY | RECORD | CONST
	| PROGRAM | PROCEDURE | FUNCTION
	| DIV | MOD | AND
	| OR 
	| NOT
	 
	| SEMICOLON | AFFECT
	| OP of string
	| ID of string
	| NUM of int
;;

type token = 
	{
		t:basetoken;
		l:int;  (* ligne *)
		c:int;  (* colonne *)
		s:int;  (* longueur *)
	}
;;

type operator = Plus | Minus | Times | Gt | Ge | Eq | Lt | Le | Ne;;

type identificateur = string;;

type expression = 
	| Ident of identificateur
	| Int of int
	| At of expression * expression
	| Operation of expression * operator * expression
;;

type nativetype = Integer | Boolean | JillJenn;;

type ttype =
	| Native of nativetype
	| Array of int*int*ttype
	| Record of identificateur*ttype array
;;

type typedeclaration = identificateur * ttype;;

type vardeclaration = identificateur * identificateur;;

type reference =
	| Identificator of identificateur
	| ArrayElement of reference*int
;;

type statement =
	| While of expression * statement
	| IfThen of expression * statement
	| IfThenElse of expression * statement * statement
	| Affect of reference * expression
	| Pvirgule of statement * statement
;;

type procedure = {
	vardeclaration : vardeclaration;
	corps : statement
}

;;



let keywords = 	[
		"begin", BEGIN;
		"end", END;
		"while", WHILE;
		"if", IF;
		"then", THEN ;
		"else", ELSE ;
		"do", DO;
		"var", VAR;
		"type", TYPE;
		"of", OF;
		"array", ARRAY;
		"record", RECORD;
		"const", CONST;
		"program", PROGRAM;
		"procedure", PROCEDURE;
		"function", FUNCTION;
		"div", DIV;
		"mod", MOD;
		"and", AND;
		"or", OR;
		"not", NOT;
	];
;;

let constr_of_keywords_tbl = 
	let k = Hashtbl.create 25 in
	List.iter (fun (keyword, constr) -> Hashtbl.add k keyword constr) keywords;
	k
;;

let keywords_of_constr_tbl = 
	let k = Hashtbl.create 25 in
	List.iter (fun (keyword, constr) -> Hashtbl.add k constr keyword) keywords;
	k
;;

let constr_of_keywords = Hashtbl.find constr_of_keywords_tbl;;

let keywords_of_constr = Hashtbl.find keywords_of_constr_tbl;;


let print_arbre q = 
	let print_elem elem =
		let constr = elem.t in
		let chaine =
			try
				String.uppercase (keywords_of_constr constr)
			with
				Not_found -> match constr with
					| OP s -> Printf.sprintf "OP(%s)" s
					| ID s -> Printf.sprintf "ID(%s)" s
					| NUM i -> Printf.sprintf "NUM(%d)" i
					| SEMICOLON -> "SEMICOLON"
					| AFFECT -> "AFFECT"
					| _ -> failwith "Missed a constructor in print_arbre, can't know what is it"
		in
		let line, col, size = elem.l, elem.c, elem.s in
		Printf.printf "%03d %03d %03d : %s\n" line col size chaine
	in
	let cq = Queue.copy q in
	while not(Queue.is_empty cq) do
		print_elem (Queue.pop cq);
	done;
;;


let string_count car chaine =
	let n = ref 0 in
	String.iter (fun c -> if c=car then incr n) chaine;
	!n
;;


open Parser;;


}


rule build q line col = parse
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as id
  	{
  		let s=String.length id in
  		let token =
  			try
  				constr_of_keywords id (* keyword réservé *)
  			with
  				Not_found -> ID id (* identificateur quelconque *)
  		in
	  		Queue.push {t=token; l=line; c=col; s=s} q;
	  		build q line (col+s) lexbuf
  	}
  | ['0'-'9']+ as number
  	{
  		let s=String.length number in
  		Queue.push {t=NUM (int_of_string number); l=line; c=col; s=s} q;
  		build q line (col+s) lexbuf
  	}
  | "{" [^ '}']* "}" as comment
  	{
  		let s = String.length comment in
  		let nlines = string_count '\n' comment in
  		let newcol =
  			if nlines = 0 then
  				s+col
  			else
  				s+1-(String.rindex comment '\n')
  			in
  		build q (line+nlines) newcol lexbuf
  	} 
  | ":="
  	{
  		Queue.push {t=AFFECT; l=line; c=col; s=2} q;
  		build q line (col+2) lexbuf
  	} 
  | "<>" | ">=" | "<=" | "==" | ".." as op
  	{
  		let s=String.length op in
  		Queue.push {t=OP op; l=line; c=col; s=s} q;
  		build q line (col+s) lexbuf
  	} 
  | ['*' '-' '+' '<' '>' '=' '(' ')' ':' '.' '[' ']'] as op
  	 {
  		Queue.push {t=OP (String.make 1 op); l=line; c=col; s=1} q;
  		build q line (col+1) lexbuf
  	 }
  | ';'
  	{
  		Queue.push {t=SEMICOLON; l=line; c=col; s=1} q;
  		build q line (col+1) lexbuf
  	} 
  | [' ' '\t']
  	{
  		build q line (col+1) lexbuf
  	} 
  | '\n'
  	{
  		build q (line+1) 1 lexbuf
  	} 
  | _ as c
  	{
  		Printf.fprintf stderr "token '%c' unrecognized\n" c;
  		failwith "parse error"
  	} 
  | eof
  	{
  		q
  	}

{
  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    let a = build (Queue.create()) 1 1 lexbuf in
    print_arbre a;
	(*exec_arbre a*);;
  let _ = Printexc.print main ()
}
