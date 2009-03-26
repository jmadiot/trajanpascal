%{
open Types;;
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
%type <Types.body> input




%% /* Grammar rules and actions follow */


input:
	program { $1 }
;

program :
	PROGRAM ID SEMICOLON blockBody DOT { $4 }
;

blockBody :
	constantDefinitionPart_opt
	typeDefinitionPart_opt
	variableDefinitionPart_opt
	procedureDefinition_etoile
	compoundStatement
	{ B ($1, $2, $3, $4, $5) }
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
	  procedureDefinition_etoile procedureDefinition { $2::$1 }
	| empty { [] }
;

constantDefinitionPart :
	CONST constantDefinition constantDefinition_etoile { $2 :: $3 }
;

constantDefinition_etoile :
	  constantDefinition_etoile constantDefinition { $2 :: $1 }
	| empty { [] }
;

constantDefinition :
	ID EQ constant SEMICOLON { ($1, $3) }
;

constant :
	  NUM { ConstantNum $1 }
	| ID { ConstantId $1 }
;

typeDefinitionPart :
	TYPE typeDefinition typeDefinition_etoile { $2 :: $3 }
;

typeDefinition_etoile:
	  typeDefinition_etoile typeDefinition { $2 :: $1 }
	| empty { [] }
;

typeDefinition :
	ID EQ newType SEMICOLON { ($1, $3) }
;

newType :
	  newArrayType  { $1 }
	| newRecordType { RecordType $1 }
;

newArrayType :
	ARRAY BRA indexRange CKET OF ID { (fun (x,y) -> ArrayType (x,y,$6)) $3 }
;

indexRange :
	constant DDOT constant { ($1, $3) }
;

newRecordType :
	RECORD fieldList END { $2 }
;

fieldList :
	semicolon_recordSection_plus { $1 } 
;

semicolon_recordSection_plus:
	  semicolon_recordSection_plus SEMICOLON recordSection { $3 :: $1 }
	| recordSection { [$1] }
;

recordSection :
	fieldNameDefList COLON ID { ($1, $3) }
;

fieldNameDefList :
	comma_fieldNameDef_plus { $1 }
;

comma_fieldNameDef_plus:
	  comma_fieldNameDef_plus COMMA ID { $3 :: $1 }
	| ID { [$1] }
;

variableDefinitionPart :
	VAR variableDefinition_plus { $2 }
;

variableDefinition_plus:
	  variableDefinition_plus variableDefinition { $2 :: $1 }
	| variableDefinition { [$1] }
;

variableDefinition :
	variableNameDefList COLON ID SEMICOLON { ($1, $3) }
;

variableNameDefList :
	  ID { [$1] }
	| variableNameDefList COMMA ID { $3 :: $1 }
;

expression :
	  simpleExpression { $1 }
	| simpleExpression LT simpleExpression { Lt ($1, $3) }
	| simpleExpression EQ simpleExpression { Eq ($1, $3) }
	| simpleExpression GT simpleExpression { Gt ($1, $3) }
	| simpleExpression LE simpleExpression { Le ($1, $3) }
	| simpleExpression NE simpleExpression { Ne ($1, $3) }
	| simpleExpression GE simpleExpression { Ge ($1, $3) }
;

simpleExpression :
	  term { $1 }
	| PLUS term { $2 }
	| MINUS term %prec UMINUS { UMinus $2 }
	| simpleExpression PLUS term { Plus ($1, $3) }
	| simpleExpression MINUS term { Minus ($1, $3) }
	| simpleExpression OR term { Or ($1, $3) }
;

term :
	  factor { $1 }
	| term TIMES factor { Times ($1, $3) }
	| term DIV factor { Div ($1, $3) }
	| term MOD factor { Mod ($1, $3) }
	| term AND factor { And ($1, $3) }
;

factor :
	  NUM { Num $1 }
	| variableAccess { Ref $1 }
	| PAREN expression THESIS { $2 }
	| notOperator factor { Not $2 }
;

notOperator :
	NOT { }
;

variableAccess :
	  ID { VarAccess $1 }
	| variableAccess BRA expression CKET { ArrayAccess ($1, $3) }
	| variableAccess DOT ID { RecordAccess ($1, $3) }
;

statement :
	  assignmentStatement { $1 }
	| procedureStatement { $1 }
	| ifStatement { $1 }
	| whileStatement { $1 }
	| compoundStatement { CompoundStatement $1 }
	| empty { Nop }
;

empty : { };

assignmentStatement :
	variableAccess ASSIGN expression { Assignment ($1, $3) }
;

procedureStatement :
	ID actualParameterList { CallProcedure ($1, $2) }
;

actualParameterList:
	  empty { [] }
	| PAREN actualParameters THESIS { $2 }
;

actualParameters :
	  actualParameter { [$1] }
	| actualParameters COMMA actualParameter { $3 :: $1 }
;

actualParameter :
	expression { $1 }
;

ifStatement :
	  IF expression THEN statement { IfThen ($2, $4) }
	| IF expression THEN statement ELSE statement { IfThenElse ($2, $4, $6) }
;

whileStatement :
	WHILE expression DO statement { While ($2, $4) }
;

compoundStatement :
	BEGIN statement_semicolon_plus END { $2 }
;

statement_semicolon_plus:
	  statement { [$1] }
	| statement_semicolon_plus SEMICOLON statement { $3 :: $1 }
;

procedureDefinition :
	PROCEDURE ID procedureBlock SEMICOLON { (fun (x,y)->($2,x,y)) $3 }  //nimp
;

procedureBlock :
	formalParameterList_opt SEMICOLON blockBody { ($1, $3) }  //nimp
;

formalParameterList_opt :
	  PAREN parameterDefinitions THESIS { $2 }
	| empty { [] }
;

parameterDefinitions :
	  parameterDefinition { [$1] }
	| parameterDefinitions SEMICOLON parameterDefinition { $3 :: $1 }
;

parameterDefinition :
	  VAR parameterNameDefList COLON ID { ParamVar ($2, $4) }
	| parameterNameDefList COLON ID  { Param ($1, $3) }
;

parameterNameDefList :
	  ID { [$1] }
	| parameterNameDefList COMMA ID { $3 :: $1 }
;


%%

