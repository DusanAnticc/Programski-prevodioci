%{
  #include <stdio.h>
  #include <stdlib.h>
  #include "defs.h"
  #include "symtab.h"
  #include "codegen.h"

  int yyparse(void);
  int yylex(void);
  int yyerror(char *s);
  void warning(char *s);

  extern int yylineno;
  int out_lin = 0;
  char char_buffer[CHAR_BUFFER_LENGTH];
  int error_count = 0;
  int warning_count = 0;
  int var_num = 0;
  int fun_idx = -1;
  int fcall_idx = -1;
  int lab_num = -1;
  FILE *output;
  
  int tip = 0;/**/
  int indexi[128];/**/
  char cindexi[128];/**/
  int dodaj_initialize[128];/**/
  int initialize[128];/**/
  char* id;/**/
  int num=0;/**/
  int** mat;/**/
  int arg_count=1;/**/
  int arg_count_reverse=1;/**/
  int tmp = 0;
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
%token <i> _AROP
%token <i> _RELOP

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
%token <i> _AROP_AROP/**/
%token _SELECT	/**/
%token _FROM	/**/
%token _WHERE	/**/
%token _QUESTIONMARK	/**/

%type <i> num_exp exp literal ternary_operator exp2
%type <i> function_call argument rel_exp if_part for_statement1 branch_statement1

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
   } global_list function_list
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


global_list
	: /*empty*/
	| global_list global_var
	;

global_var
	: _TYPE _ID _SEMICOLON
	{
		int idx = lookup_symbol($2, GVAR); 
		if (idx != NO_INDEX) 
		{
				err("redefinition of '%s'", $2);
		} else {
			insert_symbol($2, GVAR, $1, NO_ATR, NO_ATR);
			code("\n%s:\n\t\tWORD\t1", $2);
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

        code("\n%s:", $2);
        code("\n\t\tPUSH\t%%14");
        code("\n\t\tMOV \t%%15,%%14");
      }
    _LPAREN parameter _RPAREN body
      {
        clear_symbols(fun_idx + 1);
        var_num = 0;
        
        code("\n@%s_exit:", $2);
        code("\n\t\tMOV \t%%14,%%15");
        code("\n\t\tPOP \t%%14");
        code("\n\t\tRET");
      }
  ;

parameter
  : /* empty */
      { set_atr1(fun_idx, 0); }
   |params
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
  : _LBRACKET variable_list
      {
        if(var_num)
          code("\n\t\tSUBS\t%%15,$%d,%%15", 4*var_num);
        code("\n@%s_body:", get_name(fun_idx));
      }
    statement_list _RBRACKET
  ;

variable_list
  : /* empty */
  | variable_list variable
  ;

variable	/**/
  :{
  	for(int i=0;i<128;i++)
  	{
  		dodaj_initialize[i]=-1;
  	}
  } _TYPE {tip=$2;} ids var 
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
						gen_mov($2,indexi[k]);
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
		if(lookup_symbol($1, VAR|PAR|GVAR) == NO_INDEX)
		   insert_symbol($1, VAR, tip, ++var_num, NO_ATR);
		else 
		   {
		   	err("redefinition of '%s'", $1);
		   }
		int idx = lookup_symbol($1, VAR|PAR|GVAR);
		
		
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
			int idx = lookup_symbol($3, VAR|PAR|GVAR);
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
  {
  	int index=lookup_symbol($1, VAR|PAR|GVAR);
        
        int t1 = get_type(index);  	
  	code("\n\t\t%s\t", ar_instructions[$2 + (t1 - 1) * AROP_NUMBER]);
  	gen_sym_name(index);
  	code(",");
  	code("$1,");
  	gen_sym_name(index);
  	free_if_reg(index);
  }
  ;
  

compound_statement
  : _LBRACKET statement_list _RBRACKET
  ;


assignment_statement
  : _ID _ASSIGN num_exp _SEMICOLON
  {
        int idx = lookup_symbol($1, VAR|PAR|GVAR/**/);
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
	gen_mov($3,idx);/**/            
  }
  ;

num_exp
  : exp
  | num_exp _AROP exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: arithmetic operation");
        int t1 = get_type($1);    
        code("\n\t\t%s\t", ar_instructions[$2 + (t1 - 1) * AROP_NUMBER]);
        gen_sym_name($1);
        code(",");
        gen_sym_name($3);
        code(",");
        free_if_reg($3);
        free_if_reg($1);
        $$ = take_reg();
        gen_sym_name($$);
        set_type($$, t1);
      }
  ;

exp
  : literal
  | _ID
      {
        $$ = lookup_symbol($1, VAR|PAR|GVAR);
        if($$ == NO_INDEX)
          err("'%s' undeclared", $1);
      }

  | function_call
      {
        $$ = take_reg();
        gen_mov(FUN_REG, $$);
      }
  
  | _LPAREN num_exp _RPAREN
      { $$ = $2; }
  | _ID _AROP_AROP
  {  	
        int index=lookup_symbol($1, VAR|PAR|GVAR);
        
        int t1 = get_type(index); 
        int ret=take_reg();
        gen_mov(index,ret); 	
  	code("\n\t\t%s\t", ar_instructions[$2 + (t1 - 1) * AROP_NUMBER]);
  	gen_sym_name(index);
  	code(",");
  	code("$1,");
  	gen_sym_name(index);
  	free_if_reg(index);
  	$$ = ret;
  	set_type($$, t1);
  	/*$$=lookup_symbol($1, VAR|PAR);*/
  }
  |ternary_operator
  ;
  
exp2
  :literal
  | _ID
  {
  	$$ = lookup_symbol($1, VAR|PAR|GVAR);
        if($$ == NO_INDEX)
          err("'%s' undeclared", $1);
  }
  ;  
  
ternary_operator
  : _LPAREN num_exp
  {
	$<i>$ = ++lab_num;
	code("\n@if%d:", lab_num);
  }
   _RELOP num_exp
  {
  	if(get_type($2)!=get_type($5))
  		err("invalid operands in relation operator");
  	$<i>$ = $4+((get_type($2) - 1 ) * RELOP_NUMBER);
  	gen_cmp($2,$5);	
  } _RPAREN 
  {
    	code("\n\t\t%s\t@false%d", opp_jumps[$<i>6], $<i>3); 
        code("\n@true%d:", $<i>3);
        tmp = take_reg();
  } _QUESTIONMARK exp2 _COLON exp2
  {
  	gen_mov($10,tmp);
  	
  	code("\n\t\tJMP \t@exit%d", $<i>3);
        code("\n@false%d:", $<i>3);  
        	
  	gen_mov($12,tmp);
  	code("\n@exit%d:", $<i>3);
  	
  	if(get_type($10)!=get_type($12))
  		err("different type in ternary operator");
  	$$=tmp;
  }
  ;
  
cond_exp
  : exp _RELOP exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: arithmetic operation");
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
      	  
      	/*OVO I OVAJ KOD ISPOD RADE ISTU STVAR AKO BUDE PROBLEMA ISPRAVI 		MOJ KOD*/  
      	if(counter != $4)
          	err("wrong number of args to function '%s' '%d'!='%d' ", 
              get_name(fcall_idx),get_atr1(fcall_idx),$4);       
        arg_count=1;
        
       /*SA VJEZBI*/           
        if(get_atr1(fcall_idx) != $4)
          err("wrong number of arguments");
        code("\n\t\t\tCALL\t%s", get_name(fcall_idx));
        if($4 > 0)
          code("\n\t\t\tADDS\t%%15,$%d,%%15", $4 * 4);
        set_type(FUN_REG, get_type(fcall_idx));
        $$ = FUN_REG;
      }
  ;

argument
  : /* empty */
    { $$ = 0; }
  | num_exp
    { 
    /*ISTA STVAR KAO IZNAD
    MOJ KOD*/
    if(mat[fcall_idx][arg_count]!=get_type($1))
  		err("incompatible type in function call '%d'",arg_count);
    arg_count++;
    
    /*KOD SA VJEZBI*/
      if(get_atr2(fcall_idx) != get_type($1))
        err("incompatible type for argument");
      free_if_reg($1);
      code("\n\t\t\tPUSH\t");
      gen_sym_name($1);
      $$ = 1;
    }
  | argument _COMMA num_exp /* PO UZORU NA OVO IZNAD URADITI DODAVANJE OVE FUNKCIJE I GLUPOSTI */
  /*OVO MOZE DA PRAVI PROBLEM, AL VALJDA NECE :D*/
  {
  	
  	if(mat[fcall_idx][arg_count]!=get_type($3))
  		err("incompatible type in function call '%d','%d'-'%d'",arg_count,mat[fcall_idx][arg_count],get_type($3));
  	arg_count++;
  	$$=$1 +1;  		
  }
  ;

if_statement
  : if_part %prec ONLY_IF
      { code("\n@exit%d:", $1); }

  | if_part _ELSE statement
      { code("\n@exit%d:", $1); }
  ;

if_part
  : _IF _LPAREN
      {
        $<i>$ = ++lab_num;
        code("\n@if%d:", lab_num);
      }
    rel_exp
      {
        code("\n\t\t%s\t@false%d", opp_jumps[$4], $<i>3); 
        code("\n@true%d:", $<i>3);
      }
    _RPAREN statement
      {
        code("\n\t\tJMP \t@exit%d", $<i>3);
        code("\n@false%d:", $<i>3);
        $$ = $<i>3;
      }
  ;

rel_exp
  : num_exp _RELOP num_exp
      {
        if(get_type($1) != get_type($3))
          err("invalid operands: relational operator");
        $$ = $2 + ((get_type($1) - 1) * RELOP_NUMBER);
        gen_cmp($1, $3);
      }
  ;



return_statement
  : _RETURN num_exp _SEMICOLON
      {
        if(get_type(fun_idx) != get_type($2))
          err("incompatible types in return");
        gen_mov($2, FUN_REG);
        code("\n\t\tJMP \t@%s_exit", get_name(fun_idx));        
      }
  | _RETURN _SEMICOLON
      {      	
        if(get_type(fun_idx) != VOID)   
        	err("incompatible types in return"); 
        code("\n\t\tJMP \t@%s_exit", get_name(fun_idx));     
      }
  ;

for_statement
  :for_statement1
  {
  	code("\n@exit%d:",$1);
  	
  }
  ;
   
for_statement1
  : _FOR _LPAREN
  {
  	
  	$<i>$=++lab_num;
  	code("\n@for%d:",lab_num);

  } _TYPE _ID _ASSIGN literal _COLON literal
  {
  	if(lookup_symbol($5, VAR|PAR) == NO_INDEX)
	   insert_symbol($5, VAR, $4, ++var_num, NO_ATR);
	else 
	{
	   err("redefinition of '%s'", $5);
	}
	char *first=get_name($7);
  	char *second=get_name($9);
  	int f=atoi(first);
  	int s=atoi(second);
  	if($4!=get_type($7)||$4!=get_type($9))
  		err("incompatible types in for loop");
  	if(f<s)
  		err("firs literal must be greater than the second %d %d",f,s);
  		
  	int prviReg=take_reg();
  	int drugiReg=take_reg();
  	
  	int idx = lookup_symbol($5, VAR|PAR);
  	
  	
  	code("\n\t\tMOV \t$%d,",f);
  	gen_sym_name(idx);
  	code("\n\t\tMOV \t$%d,%%%d",s,drugiReg);
  	
  	code("\n@for_init%d:",$<i>3);
  	
  	  		
  } _RPAREN statement
  {
  	int idx = lookup_symbol($5, VAR|PAR);
  	char *first=get_name($7);
  	int f=atoi(first);
  	int prviReg=take_reg();
  	code("\n@for_statement%d:",$<i>3); 
  	 	
  	int drugiReg=take_reg();
  	char *second=get_name($9);
  	int s=atoi(second);
  	 	
  	code("\n\t\tSUBS \t");
  	gen_sym_name(idx);
  	code(",$1,");
  	gen_sym_name(idx);
  	  	 	
  	code("\n\t\tCMPS \t");
  	gen_sym_name(idx);
  	code(",%%%d",1);
  	code("\n\t\tJGTS\t@for_init%d",$<i>3);
  	
  		
  	clear_symbols(idx);  	
  	$$ = $<i>3;
  	
  }
  ;

branch_statement
  :branch_statement1
  {
  	code("\n@exit%d:",$1);
  }
  ;
  
branch_statement1
  : _BRANCH _LPAREN
  {
  	$<i>$=++lab_num;
  	code("\n@branch%d:",lab_num);
  } _ID _SEMICOLON literal _COMMA literal _COMMA literal _RPAREN
  {
  	if(lookup_symbol($4,VAR|PAR )==NO_INDEX)
   	 	err("define variable '%s'", $4);
  	
  	int idx = lookup_symbol($4, VAR|PAR|GVAR);
  	if(get_type(idx)!=get_type($6)||get_type(idx)!=get_type($8)||get_type(idx)!=get_type($10))
  	{
  		err("incompatible types in branch statement");
  	}  
  	
  	int prviReg=take_reg();
  	int drugiReg=take_reg();
  	int treciReg=take_reg();
  	
  	char *first=get_name($6);
  	char *second=get_name($8);
  	char *third=get_name($10);
  	  
  	int f=atoi(first);
  	int s=atoi(second);
  	int t=atoi(third);	
  	   
  	code("\n\t\tMOV \t$%d,%%%d",f,prviReg);
  	code("\n\t\tMOV \t$%d,%%%d",s,drugiReg);
  	code("\n\t\tMOV \t$%d,%%%d",t,treciReg);
  	
  	code("\n\t\tCMPS \t");
  	gen_sym_name(idx);
  	code(",%%%d",0);
  	code("\n\t\tJEQ\t@branch%d_statement1",$<i>3);
  	
  	code("\n\t\tCMPS \t");
  	gen_sym_name(idx);
  	code(",%%%d",1);
  	code("\n\t\tJEQ\t@branch%d_statement2",$<i>3);
  	
  	code("\n\t\tCMPS \t");
  	gen_sym_name(idx);
  	code(",%%%d",2);
  	code("\n\t\tJEQ\t@branch%d_statement3",$<i>3);
  	
  	code("\n\t\tJEQ\t@branch%d_other",$<i>3);
  	
  	code("\n@branch%d_statement1:",$<i>3); 
  	
  } _ONE statement
  {
  	code("\n\t\tJMP\t@end_branch%d",$<i>3);
  	code("\n@branch%d_statement2:",$<i>3);
  } _TWO statement
  {
  	code("\n\t\tJMP\t@end_branch%d",$<i>3);
  	code("\n@branch%d_statement3:",$<i>3); 
  } _THREE statement
  {
  	code("\n\t\tJMP\t@end_branch%d",$<i>3);
  	code("\n@branch%d_other:",$<i>3);	
  } _OTHER statement _ENDBRANCH
  {
  	code("\n@end_branch%d:",$<i>3);
  	$$ = $<i>3;
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
  output = fopen("output.asm", "w+");

  synerr = yyparse();

  clear_symtab();
  fclose(output);
  
  if(warning_count)
    printf("\n%d warning(s).\n", warning_count);

  if(error_count) {
    remove("output.asm");
    printf("\n%d error(s).\n", error_count);
  }

  if(synerr)
    return -1;  //syntax error
  else if(error_count)
    return error_count & 127; //semantic errors
  else if(warning_count)
    return (warning_count & 127) + 127; //warnings
  else
    return 0; //OK
}

