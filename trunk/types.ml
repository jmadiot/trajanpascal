
type identificateur = string;;

type constant = ConstantNum of int | ConstantId of string;;

type ttype =
	  ArrayType of constant*constant*identificateur
	| RecordType of (identificateur*identificateur) list
;;

type exp =
	  Ref of reference
	| Num of int
	| Plus of exp*exp
	| Minus of exp*exp
	| UMinus of exp
	| Div of exp*exp
	| Mod of exp*exp
	| And of exp*exp
	| Or of exp*exp
	| Not of exp
	| Times of exp*exp
	| Lt of exp*exp
	| Eq of exp*exp
	| Gt of exp*exp
	| Le of exp*exp
	| Ne of exp*exp
	| Ge of exp*exp
and reference = 
	  VarAccess of identificateur 
	| ArrayAccess of reference * exp
	| RecordAccess of reference * identificateur
;;

type paramDef =
	  ParamVar of (identificateur*identificateur) list
	| Param of (identificateur*identificateur) list
;;


type statement =
	  Assignment of reference * exp
	| CallProcedure of identificateur * (exp list)
	| IfThen of exp * statement
	| IfThenElse of exp * statement * statement
	| While of exp * statement
	| CompoundStatement of statement list (* à dérouler *)
	| Nop
;;

type procDef = 
	  identificateur
	* (paramDef list)
	* body
and body = B of
	  (identificateur * constant) list   (*constantes*)
	* (identificateur * ttype) list   (* types *)
	* ((identificateur list) * identificateur) list (* var *)
	* procDef list
	* statement list
;;

