%{
open Types;;

let pbegin = symbol_start_pos;;
let pend = Parsing.symbol_end_pos;;

let loc elem = {
    birth = symbol_start_pos();
    death = symbol_end_pos();
    e = elem
}

let iloc i elem = {
    birth = rhs_start_pos i;
    death = rhs_end_pos i;
    e = elem
}

let deloc x = (x.birth, x.death);;
let reloc (b,d) e = {
    birth = b;
    death = d;
    e = e
}

%}

%token BEGIN END WHILE IF THEN ELSE DO

%token VAR TYPE OF ARRAY RECORD CONST PROGRAM
%token PROCEDURE FUNCTION DIV MOD AND OR  NOT
     
%token SEMICOLON COLON ASSIGN DOT DDOT EQ COMMA    

%token LT EQ GT LE NE GE

%token BRA CKET
%token PAREN THESIS

%token PLUS MINUS TIMES

%token <string> ID
%token <int> NUM

%left PLUS MINUS OR
%left TIMES DIV AND MOD
%nonassoc UMINUS

%start input
%type <Types.program> input




%% /* Grammar rules and actions follow */

input:
    program { $1 }
;

program :
    PROGRAM id SEMICOLON blockBody DOT { (iloc 2 $2, iloc 4 $4) }
;

blockBody :
    constantDefinitionPart_opt
    typeDefinitionPart_opt
    variableDefinitionPart_opt
    procedureDefinition_etoile
    compoundStatement
    { B (loc $1, loc $2, loc $3, loc (List.rev $4), loc $5) }
;

id :
    ID { loc $1 }
;

constantDefinitionPart_opt:
      constantDefinitionPart { $1 }
    | empty { [] }
;

typeDefinitionPart_opt:
      typeDefinitionPart { $1 }
    | empty { [] }
;

variableDefinitionPart_opt:
      variableDefinitionPart { $1 }
    | empty { [] }
;

procedureDefinition_etoile:
      procedureDefinition_etoile procedureDefinition { ($2 :: $1) }
    | empty { [] }
;

constantDefinitionPart :
    CONST constantDefinition constantDefinition_etoile { $2 :: (List.rev $3) }
;

constantDefinition_etoile :
      constantDefinition_etoile constantDefinition { $2 :: $1 }
    | empty { [] }
;

constantDefinition :
    id EQ constant SEMICOLON { ($1, $3) }
;

constant :
      numeral { ConstantNum $1 }
    | id { ConstantId (loc $1) }
;

typeDefinitionPart :
    TYPE typeDefinition typeDefinition_etoile { $2 :: (List.rev $3) }
;

typeDefinition_etoile:
      typeDefinition_etoile typeDefinition { $2 :: $1 }
    | empty { [] }
;

typeDefinition :
    id EQ newType SEMICOLON { ($1, $3) }
;

newType :
      newArrayType  { $1 }
    | newRecordType { RecordType $1 }
;

newArrayType :
    ARRAY BRA indexRange CKET OF id { (fun (x,y) -> ArrayType (loc(x,y,$6))) $3 }
;

indexRange :
    constant DDOT constant { ($1, $3) }
;

newRecordType :
      RECORD fieldList END { loc $2 }
    | RECORD fieldList SEMICOLON END { loc $2 }
;

fieldList:
      fieldList SEMICOLON recordSection { $1 @ $3 }
    | recordSection { $1 }
;

recordSection :
    fieldNameDefList COLON id { List.map (fun v -> (v, $3)) $1 }
;

fieldNameDefList :
    comma_fieldNameDef_plus { List.rev $1 }
;

comma_fieldNameDef_plus:
      comma_fieldNameDef_plus COMMA id { $3 :: $1 }
    | id { [$1] }
;

variableDefinitionPart :
    VAR variableDefinition_plus { $2 }
;

variableDefinition_plus:
      variableDefinition_plus variableDefinition { $1 @ $2 }
    | variableDefinition { $1 }
;

variableDefinition :
    variableNameDefList COLON id SEMICOLON { (List.map ( fun x-> (x, $3) ) (List.rev $1)) }
;

variableNameDefList :
      id { [$1] }
    | variableNameDefList COMMA id { $3 :: $1 }
;

expression :
      simpleExpression { $1 }
    | simpleExpression LT simpleExpression { Lt (loc ($1, $3)) }
    | simpleExpression EQ simpleExpression { Eq (loc ($1, $3)) }
    | simpleExpression GT simpleExpression { Gt (loc ($1, $3)) }
    | simpleExpression LE simpleExpression { Le (loc ($1, $3)) }
    | simpleExpression NE simpleExpression { Ne (loc ($1, $3)) }
    | simpleExpression GE simpleExpression { Ge (loc ($1, $3)) }
;

simpleExpression :
      term { $1 }
    | PLUS term { $2 }
    | MINUS term %prec UMINUS { UMinus (loc $2) }
    | simpleExpression PLUS term { Plus (loc ($1, $3)) }
    | simpleExpression MINUS term { Minus (loc ($1, $3)) }
    | simpleExpression OR term { Or (loc ($1, $3)) }
;

term :
      factor { $1 }
    | term TIMES factor { Times (loc ($1, $3)) }
    | term DIV factor { Div (loc ($1, $3)) }
    | term MOD factor { Mod (loc ($1, $3)) }
    | term AND factor { And (loc ($1, $3)) }
;

factor :
      NUM { Num (loc $1) }
    | variableAccess { Ref (loc $1) }
    | PAREN expression THESIS { $2 }
    | notOperator factor { Not (loc $2) }
;

/*for constant definition. Otherwise UMINUS takes it cool*/
numeral :
      NUM { loc $1 }
    | MINUS NUM { loc $2 }
;

notOperator :
    NOT { }
;

variableAccess :
      id { VarAccess (loc $1) }
    | variableAccess BRA expression CKET { ArrayAccess (loc ($1, $3)) }
    | variableAccess DOT id { RecordAccess (loc ($1, $3)) }
;

statement :
      assignmentStatement { $1 }
    | procedureStatement { $1 }
    | ifStatement { $1 }
    | whileStatement { $1 }
    | compoundStatement { CompoundStatement (loc $1) }
    | empty { Nop (loc ()) }
;

empty : { };

assignmentStatement :
    variableAccess ASSIGN expression { Assignment (loc ($1, $3)) }
;

procedureStatement :
    id actualParameterList { CallProcedure (loc ($1, $2)) }
;

actualParameterList:
      empty { [] }
    | PAREN actualParameters THESIS { List.rev $2 }
;

actualParameters :
      actualParameter { [$1] }
    | actualParameters COMMA actualParameter { $3 :: $1 }
;

actualParameter :
    expression { $1 }
;

ifStatement :
      IF expression THEN statement { IfThen (loc ($2, $4)) }
    | IF expression THEN statement ELSE statement { IfThenElse (loc ($2, $4, $6)) }
;

whileStatement :
    WHILE expression DO statement { While (loc ($2, $4)) }
;

compoundStatement :
    BEGIN statement_semicolon_plus END { List.rev $2 }
;

statement_semicolon_plus:
      statement { match $1 with Nop _ -> [] | _ -> [$1] }
    | statement_semicolon_plus SEMICOLON statement { match $3 with Nop _ -> $1 | _ -> $3 :: $1 }
;

procedureDefinition :
    PROCEDURE id procedureBlock SEMICOLON {
    	(
    		fun (x,y) -> (
    			iloc 2 $2,
    			x,
    			y
    		)
    	) $3 }  //nimp
;

procedureBlock :
    formalParameterList_opt SEMICOLON blockBody { (iloc 1 $1, iloc 3 $3) }  //nimp
;

formalParameterList_opt :
      PAREN parameterDefinitions THESIS { List.rev $2 }
    | PAREN THESIS { [] }
    | empty { [] }
;

parameterDefinitions :
      parameterDefinition { $1 }
    | parameterDefinitions SEMICOLON parameterDefinition { $3 @ $1 }
;
/*
parameterDefinition :
      VAR parameterNameDefList COLON id { ParamVar (List.rev $2, $4) }
    | parameterNameDefList COLON id  { Param (List.rev $1, $3) }
;
*/

parameterDefinition :
      VAR parameterNameDefList COLON id {
      		(
      			List.map
      				(
      					fun x ->
      						let d = deloc x in
      					ParamVar (reloc d (x.e, $4))
      				)
      				(List.rev $2)
      		)
      }

    | parameterNameDefList COLON id {
      		(
      			List.map
      				(
      					fun x ->
      						let d = deloc x in
      					Param (reloc d (x.e, $3))
      				)
      				(List.rev $1)
      		)
      }

;




parameterNameDefList :
      id { [loc $1] }
    | parameterNameDefList COMMA id { (loc $3) :: $1 }
;


%%

