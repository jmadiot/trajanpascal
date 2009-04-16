type pos


type 'a loc = {
	birth : Lexing.position;
	death : Lexing.position;
	e : 'a;
}

type identificateur = string loc;;

type constant = ConstantNum of int loc | ConstantId of identificateur loc;;

type ttype =
	  ArrayType of (constant*constant*identificateur) loc
	| RecordType of (identificateur*identificateur) list loc
;;

type exp =
	  Ref of reference loc
	| Num of int loc
	| Plus of (exp*exp) loc
	| Minus of (exp*exp) loc
	| UMinus of exp loc
	| Div of (exp*exp) loc
	| Mod of (exp*exp) loc
	| And of (exp*exp) loc
	| Or of (exp*exp) loc
	| Not of exp loc
	| Times of (exp*exp) loc
	| Lt of (exp*exp) loc
	| Eq of (exp*exp) loc
	| Gt of (exp*exp) loc
	| Le of (exp*exp) loc
	| Ne of (exp*exp) loc
	| Ge of (exp*exp) loc
and reference =
	  VarAccess of identificateur loc
	| ArrayAccess of (reference * exp) loc
	| RecordAccess of (reference * identificateur) loc
;;

type paramDef =
	  ParamVar of (identificateur*identificateur) loc
	| Param of (identificateur*identificateur) loc
;;


type statement =
	  Assignment of (reference * exp) loc
	| CallProcedure of (identificateur * (exp list)) loc
	| IfThen of (exp * statement) loc 
	| IfThenElse of (exp * statement * statement) loc
	| While of (exp * statement) loc
	| CompoundStatement of statement list loc (* à dérouler ? (non ?) *)
	| Nop of unit loc
;;

type procDef = 
	  identificateur loc
	* (paramDef list) loc
	* body loc
and body = B of
	  (identificateur * constant) list loc   (*constantes*)
	* (identificateur * ttype) list loc   (* types *)
	* (identificateur * identificateur) list loc (* var *)
	* procDef list loc
	* statement list loc
;;

type program = identificateur loc * body loc;;



