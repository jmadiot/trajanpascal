open Types;;
open Pprint;;
open Typage;;

let cin = if Array.length Sys.argv > 1 then 
	open_in Sys.argv.(1) else stdin
;;
let cin_name = if Array.length Sys.argv > 1 then 
	Sys.argv.(1) else "stdin"
;;

let main () =
	let lexbuf = Lexing.from_channel cin in
	try
		lexbuf.Lexing.lex_curr_p <- {Lexing.pos_fname = cin_name; Lexing.pos_lnum=0;Lexing.pos_cnum=0;Lexing.pos_bol=0};
		let ast = Parser.input (Lexer.analyse 0) lexbuf in
		print_ast ast;
	with
		Parsing.Parse_error ->
			let p = lexbuf.Lexing.lex_curr_p in
			Printf.fprintf stderr "Syntax error at line %d, col %d\n"
				p.Lexing.pos_lnum (p.Lexing.pos_cnum-p.Lexing.pos_bol);

;;

(**                                                      
  On en est au typage : typage.ml                        
  il faudrait faire un joli makefile                     
  il faudrait renommer types.ml avec un nom idoine       
  il faudrait trouver un moyen d'afficher le code et     
  pas seulement la position dans les messages d'erreur.  
                                                       *)

main();;





(*

pour afficher l'ASR dans un fichier .dot pour un graphe en .ps :


open Obj
open Printf

let node_number = ref 0

let node_table = ref ([] : (Obj.t * int) list)

let in_table t = List.mem_assq t !node_table

let assoc_node t = List.assq t !node_table

let new_node t =
  incr node_number;
  node_table := (t,!node_number) :: !node_table;
  !node_number

let no_scan_tag = 248

let output_field c i x =
  fprintf c "<f%d> " i;
  if not (is_block x) then fprintf c "%d" (magic x)

let output_non_block c t =
  let tg = tag t in
  if tg = closure_tag then
    output_string c "<f0> closure"
  else if tg = object_tag then
    output_string c "<f0> object"
  else if tg = abstract_tag then
    output_string c "<f0> abstract" 
  else if tg = string_tag then
    fprintf c "<f0> \\\"%s\\\"" (magic t : string)
  else if tg = double_tag then
    fprintf c "<f0> %f" (magic t : float)
  else if tg = double_array_tag then
    fprintf c "<f0> double_array"
  else if tg = final_tag then
    fprintf c "<f0> final"
  else
    fprintf c "<f0> ???"
    
let output_node c n t =
  fprintf c "  \"node%d\" [ label = \"" n;
  if is_block t then
    if tag t < no_scan_tag then
      let n = size t - 1 in
      for i = 0 to n do
      	output_field c i (field t i);
      	if i < n then fprintf c " | "
      done
    else 
      output_non_block c t
  else
    fprintf c "%d" (magic t);
  fprintf c "\" shape = \"record\" ];\n"

let output_edge c (n1,f1) n2 =
  fprintf c "  \"node%d\":f%d -> \"node%d\":f0 [ ];\n" n1 f1 n2

let rec traverse c t =
  if not (in_table t) then begin
    let n = new_node t in
    output_node c n t;
    if is_block t && tag t < no_scan_tag then begin
      for i = 0 to size t - 1 do
      	let f = field t i in
      	if is_block f then traverse c f
      done;
      for i = 0 to size t - 1 do
      	let f = field t i in
      	if is_block f then output_edge c (n,i) (assoc_node f)
      done
    end
  end
    

let reset () =
  node_number := 0;
  node_table := []

let produce_file file f x =
  let c = open_out file in
  output_string c "digraph g {\n
  graph [ rankdir = \"LR\" ];
  node [ fontsize = \"16\" shape = \"ellipse\" ];
  edge [ ];\n\n";
  f c x;
  output_string c "\n\n}\n";
  close_out c

let display f x =
  reset();
  produce_file f traverse (repr x)

let display_list f l =
  produce_file f
    (fun c -> List.iter (fun x -> traverse c (repr x))) l
;;

let obj =
	let lexbuf = Lexing.from_channel cin in
	Parser.input (Lexer.analyse 0) lexbuf
;;

display Sys.argv.(2) obj;;





*)













