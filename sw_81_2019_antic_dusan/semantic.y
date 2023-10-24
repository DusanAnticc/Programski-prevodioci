%{
  #include <stdio.h>
  #include <stdlib.h>
  #include "defs.h"
  #include "symtab.h"

  int yyparse(void);
  int yylex(void);
  int yyerror(char *s);
  void warning(char *s);

  extern int yylineno;
  char char_buffer[CHAR_BUFFER_LENGTH];
  int error_count = 0;
  int warning_count = 0;
  int var_num = 0;
  int fun_idx = -1;
  int fcall_idx = -1;
  int tip = 0;
  int indexi[128];
  char cindexi[128];
  int dodaj_initialize[128];
  int initialize[128];
  char* id;
  int num=0;
  int** mat;
  int arg_count=1;
  int arg_count_reverse=1;
%}

%union {
  int i;
  char *s;
}

%token <i> _TYPE
%token _IF
%token _ELSE
%token _RETURN
%token <s> _ID
%token <s> _INT_NUMBER
%token <s> _UINT_NUMBER
%token _LPAREN
%token _RPAREN
%token _LBRACKET
%token _RBRACKET
%token _ASSIGN
%token _SEMICOLON
%token _COLON/**/
%token _COMMA /**/
%token _FOR /**/
%token _WHILE /**/
%token _BRANCH /**/
%token _ENDBRANCH /**/
%token _ONE /**/
%token _TWO /**/
%token _THREE /**/
%token _OTHER /**/
%token <i> _AROP
%token <i> _RELOP
%token _AROP_AROP/**/
%token _SELECT	/**/
%token _FROM	/**/
%token _WHERE	/**/

%type <i> num_exp exp literal function_call argument rel_exp

%nonassoc ONLY_IF
%nonassoc _ELSE

%%

program
  :{
  	for(int i=0;i<128;i++)
  	{
         	initialize[i]=-1;
         	indexi[i]=-1;
         	cindexi[i]=-1;
         	dodaj_initialize[i]=-1;
         }
   } function_list
      {  
        if(lookup_symbol("main", FUN) == NO_INDEX)
          err("undefined reference to 'main'");
          
        for(int i=0;i<128;i++)
  	{
         	
         	if(indexi[i]!=-1)
         	{
         		if(initialize[i]==-1)
         			err("variable %c not initialize",cindexi[i]);
         	}
         }
       }
  ;

function_list
  : function
  | function_list function
  ;

function
  : _TYPE _ID
      {
        fun_idx = lookup_symbol($2, FUN);
        if(fun_idx == NO_INDEX)
          {
          	fun_idx = insert_symbol($2, FUN, $1, NO_ATR, NO_ATR);
          	mat[fun_idx][0]=fun_idx;
          }
        else 
          err("redefinition of function '%s'", $2);
      }
    _LPAREN parameter _RPAREN body
      {
        clear_symbols(fun_idx + 1);
        var_num = 0;
      }
  ;

parameter
  : /* empty */
      { set_atr1(fun_idx, 0); }

  | params
  ;
  
params
  : _TYPE _ID
      {
      
        for(int i=1;i<128;i++)
        {
        	if(mat[fun_idx][i]==0)
        		{
        			mat[fun_idx][i]= $1;
        			break;
        		}
        }
        
        insert_symbol($2, PAR, $1, 1, NO_ATR);
        set_atr1(fun_idx, 1);
        set_atr2(fun_idx, $1);
      }
  | params _COMMA _TYPE _ID
      {
      
      	for(int i=1;i<128;i++)
        {
        	if(mat[fun_idx][i]==0)
        		{
        			mat[fun_idx][i]= $3;
        			break;
        		}
        }
        
        insert_symbol($4, PAR, $3, 1, NO_ATR);
        set_atr1(fun_idx, 1);
        set_atr2(fun_idx, $3);
      }
  ;

body
  : _LBRACKET variable_list statement_list _RBRACKET
  ;

variable_list
  : /* empty */
  | variable_list variable
  ;

variable	/**/
  :{for(int i=0;i<128;i++){dodaj_initialize[i]=-1;}} _TYPE {tip=$2;} ids var 
  ; 
  
var
  : _SEMICOLON 
  {
	  for(int k=0;k<128;k++)
		{
		   dodaj_initialize[k]=-1;			
		} 
  }
  | _ASSIGN num_exp _SEMICOLON
  {
  	if(tip == get_type($2))
	{
		for(int i=0;i<128;i++)
		{
			if(dodaj_initialize[i]!= -1)
			{
				for(int k=0;k<128;k++)
				{
					if(indexi[k]==dodaj_initialize[i])
					{
						initialize[k]=1;
						break;
					}
					
				}
			}
		}
	}
	else
		err("different types" );
		
  }
  ;

ids		/**/
  : _ID
  {
  	if(tip!=VOID)
	{
		if(lookup_symbol($1, VAR|PAR) == NO_INDEX)
		   insert_symbol($1, VAR, tip, ++var_num, NO_ATR);
		else 
		   {
		   	err("redefinition of '%s'", $1);
		   }
		int idx = lookup_symbol($1, VAR|PAR);
		
		
		for(int i=0;i<128;i++)
			{
				if(dodaj_initialize[i]==-1)
				{
					cindexi[i]=get_name(idx)[0];
					dodaj_initialize[i]=idx;
					indexi[i]=idx;
					break;
				}
			}
		
		
	}
	else
		err("variable '%s' can't be type VOID", $1 );
		
  }
  | ids _COMMA _ID
  {
	if(tip!=VOID)
	{
		if(lookup_symbol($3, VAR|PAR) == NO_INDEX)
		{
			insert_symbol($3, VAR,  tip, ++var_num, NO_ATR);
			int idx = lookup_symbol($3, VAR|PAR);
			for(int i=0;i<128;i++)
			{
				if(dodaj_initialize[i]==-1)
				{
					cindexi[i]=get_name(idx)[0];
					dodaj_initialize[i]=idx;
					indexi[i]=idx;
					break;
				}
			}
		}
		   
		else 
		   err("redefinition of '%s'", $3 );
	}
	else
		err("variable '%s' can't be type VOID", $3 );
  }
  ;
  
statement_list
  : /* empty */
  | statement_list statement
  ;

statement
  : compound_statement
  | assignment_statement
  | if_statement
  | return_statement
  | for_statement
  | while_statement
  | branch_statement
  | select_statement
  | _ID _AROP_AROP _SEMICOLON
  ;



compound_statement
  : _LBRACKET statement_list _RBRACKET
  ;

assignment_statement
  : _ID _ASSIGN num_exp _SEMICOLON
  {
        int idx = lookup_symbol($1, VAR|PAR);
        if(idx == NO_INDEX)
          err("invalid lvalue '%s' in assignment", $1);
        else
          if(get_type(idx) != get_type($3))
            err("incompatible types in assignment");
          else
	   {	   	
	   	for(int i=0;i<128;i++)
		{
			if(indexi[i]== idx)
			{
				initialize[i]=1;
				break;
			}
		}	   	
	   }            
      }
  ;


num_exp
  : exp
  | num_exp _AROP exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: arithmetic operation");
      }
  ;

cond_exp
  : exp _RELOP exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: arithmetic operation");
      }

exp
  : literal
  | _ID
      {
        $$ = lookup_symbol($1, VAR|PAR);
        if($$ == NO_INDEX)
          err("'%s' undeclared", $1);
      }
  | function_call
  | _LPAREN num_exp _RPAREN
      { $$ = $2; }
  | _ID _AROP_AROP
  {
  	$$=lookup_symbol($1, VAR|PAR);
  }
  ;

literal
  : _INT_NUMBER
      { $$ = insert_literal($1, INT); }

  | _UINT_NUMBER
      { $$ = insert_literal($1, UINT); }
  ;

function_call
  : _ID 
      {
        fcall_idx = lookup_symbol($1, FUN);
        if(fcall_idx == NO_INDEX)
          err("'%s' is not a function", $1);
      }
    _LPAREN argument _RPAREN
      {
        int counter=0;
      	for(int i=1;i<128;i++)
      	{
      		if(mat[fcall_idx][i]!=0)
      			counter++;
      	}
        
        if(counter != $4)
          err("wrong number of args to function '%s' '%d'!='%d' ", 
              get_name(fcall_idx),get_atr1(fcall_idx),$4);
        set_type(FUN_REG, get_type(fcall_idx));
        $$ = FUN_REG;
        arg_count=1;
      }
  ;

argument
  : /* empty */
    { $$ = 0; }

  | num_exp
    { 
        if(mat[fcall_idx][arg_count]!=get_type($1))
  		err("incompatible type in function call '%d'",arg_count);
      /*if(get_atr2(fcall_idx) != get_type($1))
        err("incompatible type for argument in '%s'",
            get_name(fcall_idx));*/
      arg_count++;
      $$ = 1;
    }
  | argument _COMMA num_exp
  {
  	
  	if(mat[fcall_idx][arg_count]!=get_type($3))
  		err("incompatible type in function call '%d','%d'-'%d'",arg_count,mat[fcall_idx][arg_count],get_type($3));
  	arg_count++;
  	$$=$1 +1;
  	
  		
  }
  ;

if_statement
  : if_part %prec ONLY_IF
  | if_part _ELSE statement
  ;

if_part
  : _IF _LPAREN rel_exp _RPAREN statement
  ;

rel_exp
  : num_exp _RELOP num_exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: relational operator");
      }
  ;

return_statement
  : _RETURN num_exp _SEMICOLON
      {
        if(get_type(fun_idx) != get_type($2))
          err("incompatible types in return");
      }
  | _RETURN _SEMICOLON
      {      	
        if(get_type(fun_idx) != VOID)   
        	err("incompatible types in return");     
      }
  ;
  
for_statement
  : _FOR _LPAREN _TYPE _ID 
  {
    if(lookup_symbol($4, VAR|PAR) == NO_INDEX)
		   insert_symbol($4, VAR, $3, ++var_num, NO_ATR);
		else 
		   {
		   	err("redefinition of '%s'", $4);
		   }
  } _ASSIGN literal _COLON literal _RPAREN statement
  {
  char *first=get_name($7);
  char *second=get_name($9);
  int f=atoi(first);
  int s=atoi(second);
  	if($3!=get_type($7)||$3!=get_type($9))
  		err("incompatible types in for loop");
  	if(f<s)
  		err("firs literal must be greater than the second %d %d",f,s);
  	
  	int idx = lookup_symbol($4, VAR|PAR);
  	clear_symbols(idx);
  }	
  ;
  
branch_statement
  : _BRANCH _LPAREN _ID _SEMICOLON literal _COMMA literal _COMMA literal _RPAREN _ONE statement _TWO statement _THREE statement _OTHER statement _ENDBRANCH {
  if(lookup_symbol($3,VAR|PAR )==NO_INDEX)
  {
    	err("define variable '%s'", $3);
  }
  else{
  int idx = lookup_symbol($3, VAR|PAR);
  if(get_type(idx)!=get_type($5)||get_type(idx)!=get_type($7)||get_type(idx)!=get_type($9))
  {
  	err("incompatible types in branch statement");
  }  
  }   

  }                                 
  ;
  
select_statement
  : _SELECT select_part _FROM _ID _WHERE _LPAREN rel_exp _RPAREN _SEMICOLON
  ;
 
select_part
  : _ID
  | select_part _COMMA _ID
  ;

while_statement
  : _WHILE _LPAREN cond_exp _RPAREN statement
  ;

%%

int yyerror(char *s) {
  fprintf(stderr, "\nline %d: ERROR: %s", yylineno, s);
  error_count++;
  return 0;
}

void warning(char *s) {
  fprintf(stderr, "\nline %d: WARNING: %s", yylineno, s);
  warning_count++;
}

int main() {

  mat =(int**) malloc ( 128 * sizeof(int*));

  for(int i=0;i<128;i++)
  {
  	mat[i]=(int*) malloc (128*sizeof(int));
  }
  
  for(int i=0;i<128;i++)
  {
  	for(int j=0;j<128;j++)
	  {
	  	mat[i][j]=0;
	  }
  }
  int synerr;
  init_symtab();

  synerr = yyparse();

  clear_symtab();
  
  if(warning_count)
    printf("\n%d warning(s).\n", warning_count);

  if(error_count)
    printf("\n%d error(s).\n", error_count);

  
  if(synerr)
    return -1; //syntax error
  else
    return error_count; //semantic errors
}

