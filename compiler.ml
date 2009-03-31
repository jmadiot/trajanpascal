open Types;;


let print_tab n = print_string (String.make n '\t');;

let print_constant = function
	| ConstantNum i -> print_int i;
	| ConstantId s -> Printf.printf "C(%s)" s
;;

let print_ttype depth = function
	  ArrayType (a,b,id) ->
	  	print_string "array[";
	  	print_constant a;
	  	print_string "..";
	  	print_constant b;
	  	print_string ("] of " ^ id);
	| RecordType idstypes ->
		print_newline();
		print_tab (depth+1);
		print_endline "RECORD";
		List.iter
		(function (id, t) ->
			print_tab (depth+2);
			print_string (id ^ " : " ^ t ^ "\n");
		)
		idstypes;
		print_tab (depth+1);
		print_endline "END";
;;

let rec print_ref = function
	  VarAccess id -> print_string ("R("^id^")");
	| ArrayAccess (r, e) -> print_ref r; print_string "["; print_expr e; print_string "]";
	| RecordAccess (r, id) -> print_ref r; print_string "."; print_string id;
and print_expr = function
	  Ref r -> print_ref r
	| Num i -> print_int i
	| Plus (e, f)  -> print_string "("; print_expr e; print_string "+"; print_expr f; print_string ")";
	| Minus (e, f) -> print_string "("; print_expr e; print_string "-"; print_expr f; print_string ")";
	| UMinus e -> print_string "(-"; print_expr e; print_string ")";
	| Not e -> print_string "(!"; print_expr e; print_string ")";
	| Div (e, f) -> print_string "("; print_expr e; print_string "/"; print_expr f; print_string ")";
	| Mod (e, f) -> print_string "("; print_expr e; print_string "%"; print_expr f; print_string ")";
	| And (e, f) -> print_string "("; print_expr e; print_string "&"; print_expr f; print_string ")";
	| Or  (e, f) -> print_string "("; print_expr e; print_string "|"; print_expr f; print_string ")";
	| Times(e, f)-> print_string "("; print_expr e; print_string "*"; print_expr f; print_string ")";
	| Lt  (e, f) -> print_string "("; print_expr e; print_string "<"; print_expr f; print_string ")";
	| Le  (e, f) -> print_string "("; print_expr e; print_string "≤"; print_expr f; print_string ")";
	| Gt  (e, f) -> print_string "("; print_expr e; print_string ">"; print_expr f; print_string ")";
	| Ge  (e, f) -> print_string "("; print_expr e; print_string "≥"; print_expr f; print_string ")";
	| Eq  (e, f) -> print_string "("; print_expr e; print_string "="; print_expr f; print_string ")";
	| Ne  (e, f) -> print_string "("; print_expr e; print_string "<>"; print_expr f; print_string ")";
;;

let rec print_statement depth = function
	Assignment (r, e) ->
		print_tab depth;
		print_ref r;
		print_string " := ";
		print_expr e;
		print_string "\n";
	| CallProcedure (id, exprs) ->
		print_tab depth;
		print_string (id^"(");
		List.iter (fun e -> print_expr e; print_string " ") exprs;
		print_string (")\n");
	| IfThen (e, th) ->
		print_tab depth;
		print_string ("IF ");
		print_expr e;
		print_string (" THEN\n");
		print_statement (depth+1) th;
	| IfThenElse (e, th, el) ->
		print_tab depth;
		print_string ("IF ");
		print_expr e;
		print_string (" THEN\n");
		print_statement (depth+1) th;
		print_tab depth;
		print_string ("ELSE\n");
		print_statement (depth+1) el;
	| While (e, s) ->
		print_tab depth;
		print_string ("WHILE ");
		print_expr e;
		print_string (" DO\n");
		print_statement (depth+1) s;
	| CompoundStatement statements ->
		List.iter (print_statement (depth)) statements;
	| Nop ->
		print_tab depth;
		print_endline "NOP !\n";
;;

let print_param = function
	  ParamVar params ->
	  	List.iter (function (v,t) -> Printf.printf "VAR %s : %s, " v t) params;
	| Param params->
	  	List.iter (function (v,t) -> Printf.printf "VAR %s : %s, " v t) params;
;;

let print_body =
	let rec print_body_depth depth (B (c, t, v, p, s)) =
		(match c with
			[] -> ()
			| _ ->
				print_tab depth;
				print_endline "CONSTS :";
				List.iter (
					print_tab depth;
					fun (id,c) -> print_string ("\t"^id^" : ");
					print_constant c;
					print_newline();
				) c
		);
		(match t with
			[] -> ()
			| _ ->
				print_tab depth;
				print_endline "TYPES :";
				List.iter (
					print_tab depth;
					fun (id,t) -> print_string ("\t"^id^" : ");
					print_ttype depth t;
					print_newline();
				) t
		);
		(match v with
			[] -> ()
			| _ ->
				print_tab depth;
				print_endline "VARS :";
				List.iter (
					fun (ids,t) ->
						print_tab depth;
						print_string "\t";
						List.iter ( fun id -> print_string (id^" ")) ids;
						print_string ": ";
						print_string t;
						print_newline();
				) v
		);
		(match p with
			[] -> ()
			| _ ->
				print_tab depth;
				print_endline "PROCEDURES :";
				List.iter (
					fun (id, params, body) ->
						print_tab (depth+1);
						print_string (id ^ "(");
						List.iter 
							(*( fun _ -> print_string "PARAMS")*)
							print_param
							params;
						print_string ")\n";
						print_body_depth (depth+1) body;
				) p
		);
		(match s with
			[] -> ()
			| _ ->
				print_tab depth;
				print_endline "CS :";
				List.iter (print_statement (depth+1)) s
		);
	in
		print_body_depth 0
;;










let cin =
	if Array.length Sys.argv > 1 then
		open_in Sys.argv.(1)
	else
		stdin
;;

exception E of body;;

let main () =
	let lexbuf = Lexing.from_channel cin in
	try
		let ast = Parser.input (Lexer.analyse 0) lexbuf in
		print_body ast
	with
		Parsing.Parse_error ->
			let p = lexbuf.Lexing.lex_curr_p in
			Printf.fprintf stderr "Syntax error at line %d, col %d\n"
				p.Lexing.pos_lnum (p.Lexing.pos_cnum-p.Lexing.pos_bol);

;;

main();;

