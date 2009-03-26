{

open Parser;;

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


let constr_of_keywords = Hashtbl.find constr_of_keywords_tbl;;

let string_count car chaine =
	let n = ref 0 in
	String.iter (fun c -> if c=car then incr n) chaine;
	!n
;;


let addlines lexbuf n =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + n;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
  ;;


}


rule analyse = parse
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as id
	{
		try
			constr_of_keywords id (* keyword réservé *)
		with
			Not_found -> ID id (* identificateur quelconque *)
	}
	| ['0'-'9']+ as number
		{ NUM (int_of_string number)}
	| "{" [^ '}']* "}" as comment
		{
			let nlines = string_count '\n' comment in
			let _ = addlines lexbuf nlines in
			analyse lexbuf
		} 
	| ":=" {ASSIGN} 
	
	| ';' {SEMICOLON}
	| ':' {COLON} 
	| ',' {COMMA}
	| ".." {DDOT}
	| '.' {DOT}
	
	| '+' {PLUS} | '-' {MINUS}
	| '*' {TIMES}
	
	| "<="{LE} | "<>"{NE} | ">="{GE}
	| '<' {LT} | '=' {EQ} | '>' {GT}
	
	| '(' {PAREN}
	| ')' {THESIS}
	
	| '[' {BRA}
	| ']' {CKET}
	
	| [' ' '\t'] {analyse lexbuf} 
	| '\n'
		{
			let _ = addlines lexbuf 1 in
			analyse lexbuf
		} 
	| _ as c
		{
			let _ = Printf.fprintf stderr "token '%c' unrecognized\n" c in
			failwith "lexical error"
		} 
	| eof
		{raise End_of_file}

{
}
