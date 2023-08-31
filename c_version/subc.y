%{
/*
 * File Name   : subc.y
 * Description : a skeleton bison input
 */

#include "subc.h"

int    yylex ();
int    yyerror (char* s);

static type *current_return_type;
static id *current_function_name;
static int num_of_strings;
static int num_of_for_predicates;
static int num_of_for_stmts;
%}

/* yylval types */
%union {
        int         intVal;
        char        charVal;
        expr        expr;
        function   *fun;
        type       *type;
        arg_list   *args;
        id         *id;
}

/* Precedences and Associativities */
%nonassoc NOELSE
%nonassoc ELSE
%left ','
%right '='
%left LOGICAL_OR
%left LOGICAL_AND
%left '&'
%left EQUOP
%left RELOP
%left '+' '-'
%left '*' '/' '%'
%right '!' INCOP DECOP
%left '[' ']' '(' ')' '.' STRUCTOP

/* Token and Types */
%token                  VOID STRUCT STRUCTOP NULLPTR
%token                  IF ELSE WHILE FOR RETURN BREAK CONTINUE
%token                  INCOP DECOP LOGICAL_AND LOGICAL_OR
%token<charVal>         CHAR_CONST
%token<id>              ID STRING RELOP EQUOP TYPE
%token<intVal>          INTEGER_CONST
%type<intVal>           pointers struct_def
%type<fun>              func_decl
%type<args>             args
%type<expr>             const_expr expr or_expr or_list and_expr and_list binary unary
%type<type>             type_specifier struct_specifier check_lvalue
%%
program: ext_def_list {
    print_code("Lglob. data %d\n", top_data_size());
 }
   ;
ext_def_list: ext_def_list ext_def {
    /* do nothing */
 }
   | /* empty */ {
    id *write_int, *write_string, *write_char;
    enter_scope(); /* global scope */

    write_int = enter(ID, "write_int", 9);
    write_string = enter(ID, "write_string", 12);
    write_char = enter(ID, "write_char", 10);

    enter_fun(write_int);
    enter_fun(write_string);
    enter_fun(write_char);

    print_code("\tshift_sp 1\n"); // hole for return of main
    print_code("\tpush_const EXIT\n");
    print_code("\tpush_reg fp\n");
    print_code("\tpush_reg sp\n");
    print_code("\tpop_reg fp\n");
    print_code("\tjump main\n");
    print_code("EXIT:\n");
    print_code("\texit\n");
 }
   ;
ext_def: type_specifier pointers ID ';' {
    enter_var($3, $1, $2);
 }
    | type_specifier pointers ID '[' { turn_on_arr(); } const_expr ']' ';' {
    int size;

    turn_off_arr();
    if ($6.ctor == Const_int)
        size = $6.value.int_val;
    else
        size = 0;

    enter_arr($3, $1, $2, size);
 }
    | func_decl ';' {
    enter_fun(current_function_name);
    insert_function(current_function_name, $1);
 }
    | type_specifier ';' ;
    | func_decl enter_params compound_stmt {
    print_code("\tpop_reg pc\n");
 }
    ;
enter_params: /* empty */ {
    function *fun;
    env *params, *prev, *temp;

    fun = $<fun>0;
    fun->defined = True;
    enter_fun(current_function_name);
    current_return_type = fun->ret;

    print_code("%s:\n", current_function_name->name);
    enter_scope();
    /* temporarily reverse the parameter list */
    for (prev = NULL, params = fun->params; params; prev = params, params = temp) {
        temp = params->next;
        params->next = prev;
    }
    /* insert parameters while reverting the list */
    for (; prev; params = prev, prev = temp) {
        temp = prev->next;
        prev->next = params;
        update_env(prev->val->ctor, prev->key, prev->val->type, prev->val->size);
    }
    insert_function(current_function_name, fun);
    print_code("\tshift_sp %d\n", -top_data_size());
 }
    ;
type_specifier: TYPE {
    id *int_id;
    type *ret;

    int_id = enter(TYPE, "int", 3);

    if ($1 == int_id)
        ret = enter_type(Int, NULL, NULL);
    else
        ret = enter_type(Char, NULL, NULL);

    $$ = ret;
 }
    | VOID {
    $$ = enter_type(Void, NULL, NULL);
 }
    | struct_specifier {
    $$ = $1;
 }
    ;
struct_specifier: STRUCT ID '{' struct_def def_list '}' {
    env *fields;

    fields = exit_scope();
    $$ = NULL;
    if ($4) /* if already declared */
        free_scope(fields);
    else {
        insert_struct($2, fields);
        $$ = enter_type(Struct, NULL, $2);
    }
 }
    | STRUCT ID {
    $$ = NULL;
    if (find_struct($2))
        $$ = enter_type(Struct, NULL, $2);
    else
        print_error("incomplete type");
 }
    ;
struct_def: /* empty */ {
    if (($$ = find_struct($<id>-1) != NULL))
        print_error("redeclaration");
    enter_scope();
 }
    ;
func_decl: type_specifier pointers ID '(' param_def ')' {
    function *fun;

    fun = make_function($1, $2, exit_scope());
    current_function_name = $3;

    $$ = fun;
 }
    | type_specifier pointers ID '(' param_def VOID ')' {
    function *fun;

    fun = make_function($1, $2, exit_scope());
    current_function_name = $3;

    $$ = fun;
 }
    | type_specifier pointers ID '(' param_def param_list ')' {
    function *fun;

    fun = make_function($1, $2, exit_scope());
    current_function_name = $3;

    $$ = fun;
 }
    ;
param_def: /* empty */ { enter_scope(); }
    ;
pointers: '*' { $$ = 1; }
    | /* empty */ { $$ = 0; }
    ;
param_list: param_decl ;
    | param_list ',' param_decl ;
    ;
param_decl: type_specifier pointers ID {
    enter_var($3, $1, $2);
 }
    | type_specifier pointers ID '[' { turn_on_arr(); } const_expr ']' {
    int size;

    turn_off_arr();
    if ($6.ctor == Const_int)
        size = $6.value.int_val;
    else
        size = 0;

    enter_arr($3, $1, $2, size);
 }
    ;
def_list: def_list def ;
    | /* empty */ ;
    ;
def: type_specifier pointers ID ';' {
    enter_var($3, $1, $2);
 }
    | type_specifier pointers ID '[' { turn_on_arr(); } const_expr ']' ';' {
    int size;

    turn_off_arr();
    if ($6.ctor == Const_int)
        size = $6.value.int_val;
    else
        size = 0;

    enter_arr($3, $1, $2, size);
 }
    | type_specifier ';' ;
    | func_decl ';' {
    enter_fun(current_function_name);
    insert_function(current_function_name, $1);
 }
    ;
enter_compound_stmt: /* empty */ {
    print_code("\tpush_reg fp\n");
    print_code("\tpush_reg sp\n");
    print_code("\tpop_reg fp\n");
    enter_scope();
 }
    ;
compound_stmt: '{' local_defs stmt_list '}' {
    print_code("\tpush_reg fp\n");
    print_code("\tpop_reg sp\n");
    print_code("\tpop_reg fp\n");
    free_scope(exit_scope());
 }
    ;
local_defs: def_list {
    print_code("\tshift_sp %d\n", top_data_size());
 }
    ;
stmt_list: stmt_list stmt {
    /* clear stack */
    print_code("\tpush_reg fp\n");
    print_code("\tpop_reg sp\n");
    print_code("\tshift_sp %d\n", top_data_size());
 }
    | /* empty */ ;
    ;
stmt: expr ';' ;
    | enter_compound_stmt compound_stmt ;
    | RETURN ';' {
    int depth;

    if (current_return_type != enter_type(Void, NULL, NULL))
        print_error("incompatible return types");
    else {
        for (depth = current_depth(); depth > 0; --depth) {
            print_code("\tpush_reg fp\n");
            print_code("\tpop_reg sp\n");
            print_code("\tpop_reg fp\n");
        }
        print_code("\tpop_reg pc\n");
    }
 }
    | RETURN expr ';' {
    type *ret_type;
    int depth, i;

    ret_type = what_type(&($2));

    if (current_return_type != ret_type) {
        if (ret_type && current_return_type &&
            ret_type->ctor == Ptr && current_return_type->ctor == Ptr) {
            if (ret_type->arg.ptr != NULL) /* not nullptr */
                print_error("incompatible return types");
        } else
                print_error("incompatible return types");
    } else {
        /* return value assignment */
        print_code("\tpush_reg fp\n");
        for (depth = current_depth(); depth > 1; --depth) {
            print_code("\tfetch\n");
        }
        print_code("\tpush_const %d\n", type_size(current_return_type) + 1);
        print_code("\tsub\n");
        /* stack top = base addr of return hole */
        for (i = 0; i < type_size(current_return_type); ++i) {
            print_code("\tpush_reg sp\n");
            print_code("\tfetch\n");
            print_code("\tpush_reg sp\n");
            print_code("\tpush_const %d\n", type_size(current_return_type) + 1 - i);
            print_code("\tsub\n");
            print_code("\tfetch\n");
            print_code("\tassign\n");
            print_code("\tpush_const 1\n");
            print_code("\tadd\n");
        }

        for (depth = current_depth(); depth > 0; --depth) {
            print_code("\tpush_reg fp\n");
            print_code("\tpop_reg sp\n");
            print_code("\tpop_reg fp\n");
        }
        print_code("\tpop_reg pc\n");
    }
 }
    | ';' ;
    | IF enter_if_else '(' expr check_break_condition ')' stmt %prec NOELSE {
    print_code("%s:\n", current_else_label()->name);
    print_code("%s:\n", current_if_label()->name);
    exit_branch();
 }
    | IF enter_if_else '(' expr check_break_condition ')' stmt ELSE goto_if_label insert_break_label stmt {
    print_code("%s:\n", current_if_label()->name);
    exit_branch();
 }
    | WHILE enter_loop '(' insert_continue_label expr check_break_condition ')' stmt {
    print_code("\tjump %s\n", current_if_label()->name);
    print_code("%s:\n", current_else_label()->name);
    exit_branch();
 }
    | FOR enter_loop '(' expr_e ';' insert_predicate expr_e check_break_condition jump_to_stmt ';' insert_continue_label expr_e jump_to_predicate insert_stmt_label ')' stmt {
    print_code("\tjump %s\n", current_if_label()->name);
    print_code("%s:\n", current_else_label()->name);
    exit_branch();
 }
    | BREAK ';' {
    branch_stack *loop;
    int depth;

    loop = current_loop();
    if (!loop)
        print_error("cannot break");
    else {
        for (depth = current_depth(); depth > loop->depth; --depth) {
            print_code("\tpush_reg fp\n");
            print_code("\tpop_reg sp\n");
            print_code("\tpop_reg fp\n");
        }
        print_code("\tjump %s\n", loop->else_label->name);
    }
 }
    | CONTINUE ';' {
    branch_stack *loop;
    int depth;

    loop = current_loop();
    if(!loop)
        print_error("cannot continue");
    else {
        for (depth = current_depth(); depth > loop->depth; --depth) {
            print_code("\tpush_reg fp\n");
            print_code("\tpop_reg sp\n");
            print_code("\tpop_reg fp\n");
        }
        print_code("\tjump %s\n", loop->if_label->name);
    }
 }
    ;
insert_predicate: /* empty */ {
    print_code("Pred%d:\n", num_of_for_predicates);
 }
    ;
insert_stmt_label: /* empty */ {
    print_code("Stmt%d:\n", num_of_for_stmts);
    ++num_of_for_stmts;
 }
    ;
jump_to_predicate: /* empty */ {
    print_code("\tjump Pred%d\n", num_of_for_predicates);
    ++num_of_for_predicates;
 }
    ;
jump_to_stmt: /* empty */ {
    print_code("\tjump Stmt%d\n", num_of_for_stmts);
 }
    ;
enter_if_else: /* empty */ { enter_branch(0); }
    ;
enter_loop: /* empty */ { enter_branch(1); }
    ;
goto_if_label: /* empty */ { print_code("\tjump %s\n", current_if_label()->name); }
    ;
check_break_condition: /* empty */ { print_code("\tbranch_false %s\n", current_else_label()->name); }
    ;
insert_continue_label: /* empty */ { print_code("%s:\n", current_if_label()->name); }
    ;
insert_break_label: /* empty */ { print_code("%s:\n", current_else_label()->name); }
    ;
expr_e: expr ;
    | /* empty */ { print_code("\tpush_const 1\n"); }
    ;
const_expr: expr ;
    ;
expr: unary '=' check_lvalue expr {
    expr *temp;
    type *t;
    int i;

    t = $3;

    print_code("\tpush_reg sp\n");
    print_code("\tpush_const %d\n", type_size(t));
    print_code("\tsub\n");
    print_code("\tfetch\n");
    for (i = 0; i < type_size(t); ++i) {
        print_code("\tpush_reg sp\n");
        print_code("\tfetch\n");
        print_code("\tpush_reg sp\n");
        print_code("\tpush_const %d\n", type_size(t) + 1 - i);
        print_code("\tsub\n");
        print_code("\tfetch\n");
        print_code("\tassign\n");
        print_code("\tpush_const 1\n");
        print_code("\tadd\n");
    }
    print_code("\tshift_sp %d\n", -type_size(t) - 1);

    for (i = type_size(t); i > 1; --i) {
        print_code("\tpush_reg sp\n");
        print_code("\tfetch\n");
        print_code("\tpush_const 1\n");
        print_code("\tadd\n");
    }
    print_code("\tshift_sp %d\n", -type_size(t) + 1);
    print_code("\tfetch\n");
    for (i = type_size(t); i > 1; --i) {
        print_code("\tshift_sp 1\n");
        print_code("\tfetch\n");
    }

    temp = check_binary_op(Assign, &($1), &($4));
    $$ = *temp;
    free(temp);
 }
    | or_expr ;
    ;
check_lvalue: /* empty */ {
    expr *temp;

    $$ = NULL;
    temp = &($<expr>-1);
    switch(temp->ctor) {
        case Id:
            switch (temp->value.id_val->val->ctor) {
                case Arr:
                case Fun:
                    print_error("LHS is not a variable");
                    temp->ctor = Error;
                    break;
                case Var:
                    $$ = temp->value.id_val->val->type;
                    break;
            }
            break;
        case Lvalue:
            $$ = temp->value.lr_val;
            break;
        default:
            print_error("LHS is not a variable");
            temp->ctor = Error;
            break;
    }
 }
    ;
or_expr: or_list {
    /* print_break_label */
    print_code("%s:\n", current_if_label()->name);
    exit_branch();
 }
    ;
or_list: or_list LOGICAL_OR check_or and_expr {
    expr *temp;

    temp = check_binary_op(Or, &($1), &($4));
    $$ = *temp;
    free(temp);
 }
    | and_expr { enter_branch(0); }
    ;
check_or: /* empty */ {
    /* if stack top is 1, break */
    print_code("\tpush_reg sp\n");
    print_code("\tfetch\n");
    print_code("\tbranch_true %s\n", current_if_label()->name);
 }
    ;
and_expr: and_list {
    /* print_break_label */
    print_code("%s:\n", current_else_label()->name);
    exit_branch();
 }
    ;
and_list: and_list LOGICAL_AND check_and binary {
    expr *temp;

    temp = check_binary_op(And, &($1), &($4));
    $$ = *temp;
    free(temp);
 }
    | binary { enter_branch(0); }
    ;
check_and: /* empty */ {
    /* if stack top is 0, break */
    print_code("\tpush_reg sp\n");
    print_code("\tfetch\n");
    print_code("\tbranch_false %s\n", current_else_label()->name);
 }
    ;
binary: binary RELOP binary {
    expr *temp;
    char *relop;
    binary_op bop;

    relop = $2->name;
    if (relop[0] == '<') {
        if (relop[1] == '\0') {
            bop = Lt;
        } else {
            bop = Le;
        }
    } else {
        if (relop[1] == '\0') {
            bop = Gt;
        } else {
            bop = Ge;
        }
    }

    temp = check_binary_op(bop, &($1), &($3));
    $$ = *temp;
    free(temp);
 }
    | binary EQUOP binary {
    expr *temp;
    binary_op bop;

    if (*($2->name) == '!')
        bop = Neq;
    else
        bop = Eq;

    temp = check_binary_op(bop, &($1), &($3));
    $$ = *temp;
    free(temp);
 }
    | binary '*' binary {
    print_code("\tmul\n");
    $$.ctor = Rvalue;
    $$.value.lr_val = enter_type(Int, NULL, NULL);
 }
    | binary '%' binary {
    print_code("\tmod\n");
    $$.ctor = Rvalue;
    $$.value.lr_val = enter_type(Int, NULL, NULL);
 }
    | binary '/' binary {
    print_code("\tdiv\n");
    $$.ctor = Rvalue;
    $$.value.lr_val = enter_type(Int, NULL, NULL);
 }
    | binary '+' binary {
    expr *temp;

    temp = check_binary_op(Add, &($1), &($3));
    $$ = *temp;
    free(temp);
 }
    | binary '-' binary {
    expr *temp;

    temp = check_binary_op(Sub, &($1), &($3));
    $$ = *temp;
    free(temp);
 }
    | unary %prec '=' { /* convert to rvalue */
    expr *temp;
    type *var_type;
    int i;

    temp = &($1);
    switch(temp->ctor) {
        case Id:
            switch(temp->value.id_val->val->ctor) {
                case Var:
                    var_type = temp->value.id_val->val->type;
                    for (i = type_size(var_type); i > 1; --i) {
                        print_code("\tpush_reg sp\n");
                        print_code("\tfetch\n");
                        print_code("\tpush_const 1\n");
                        print_code("\tadd\n");
                    }
                    print_code("\tshift_sp %d\n", -type_size(var_type) + 1);
                    print_code("\tfetch\n");
                    for (i = type_size(var_type); i > 1; --i) {
                        print_code("\tshift_sp 1\n");
                        print_code("\tfetch\n");
                    }
                    temp->ctor = Rvalue;
                    temp->value.lr_val = var_type;
                    break;
                default:
                    temp->ctor = Error;
                    break;
            }
            break;
        case Lvalue:
            var_type = temp->value.lr_val;
            for (i = type_size(var_type); i > 1; --i) {
                print_code("\tpush_reg sp\n");
                print_code("\tfetch\n");
                print_code("\tpush_const 1\n");
                print_code("\tadd\n");
            }
            print_code("\tshift_sp %d\n", -type_size(var_type) + 1);
            print_code("\tfetch\n");
            for (i = type_size(var_type); i > 1; --i) {
                print_code("\tshift_sp 1\n");
                print_code("\tfetch\n");
            }
            temp->ctor = Rvalue;
            break;
        default:
            break;
    }

    $$ = *temp;
 }
    ;
unary: '(' expr ')' { $$ = $2; }
    | '(' unary ')' { $$ = $2; }
    | INTEGER_CONST {
    $$.ctor = Const_int;
    $$.value.int_val = $1;
    print_code("\tpush_const %d\n", $1);
 }
    | CHAR_CONST {
    $$.ctor = Const_char;
    $$.value.char_val = $1;
    print_code("\tpush_const %d\n", $1);
 }
    | STRING {
    $$.ctor = Const_string;
    $$.value.string_val = $1;
    print_code("Str%d. string %s\n", num_of_strings, $1->name);
    print_code("\tpush_const Str%d\n", num_of_strings);
    ++num_of_strings;
 }
    | NULLPTR {
    type *null = enter_type(Ptr, NULL, NULL);
    $$.ctor = Rvalue;
    $$.value.lr_val = null;
    print_code("\tpush_const 0\n");
 }
    | ID {
    env *var = find_var($1);
    int depth;

    if (var) {
        $$.ctor = Id;
        $$.value.id_val = var;
        if (var->val->ctor != Fun) {
            if (var->val->depth) { /* local */
                print_code("\tpush_reg fp\n");
                for (depth = current_depth(); depth > var->val->depth; --depth) {
                    print_code("\tfetch\n");
                }
                print_code("\tpush_const %d\n", var->val->offset + 1);
                print_code("\tadd\n");
            } else { /* global */
                print_code("\tpush_const Lglob\n");
                print_code("\tpush_const %d\n", var->val->offset);
                print_code("\tadd\n");
            }
        }
    } else
        $$.ctor = Error;
 }
    | '-' unary %prec '!' {
    expr *temp;

    temp = check_unary_op(Neg, &($2));
    $$ = *temp;
    free(temp);
 }
    | '!' unary {
    expr *temp;

    temp = check_unary_op(Not, &($2));
    $$ = *temp;
    free(temp);
 }
    | unary INCOP {
    expr *temp;

    temp = check_unary_op(Post_incr, &($1));
    $$ = *temp;
    free(temp);
 }
    | unary DECOP {
    expr *temp;

    temp = check_unary_op(Post_decr, &($1));
    $$ = *temp;
    free(temp);
 }
    | INCOP unary {
    expr *temp;

    temp = check_unary_op(Pre_incr, &($2));
    $$ = *temp;
    free(temp);
 }
    | DECOP unary {
    expr *temp;

    temp = check_unary_op(Pre_decr, &($2));
    $$ = *temp;
    free(temp);
 }
    | '&' unary %prec '!' {
    expr *temp;

    temp = check_unary_op(Ref, &($2));
    $$ = *temp;
    free(temp);
 }
    | '*' unary %prec '!' {
    expr *temp;

    temp = check_unary_op(Deref, &($2));
    $$ = *temp;
    free(temp);
 }
    | unary '[' expr ']' {
    expr *temp;

    temp = check_binary_op(Arr_access, &($1), &($3));
    $$ = *temp;
    free(temp);
 }
    | unary STRUCTOP ID {
    expr *temp, *temp_struct;

    turn_on_structop();
    temp_struct = check_unary_op(Deref, &($1));
    temp = check_field_access(temp_struct, $3);
    turn_off_structop();

    $$ = *temp;
    free(temp_struct);
    free(temp);
 }
    | unary '.' ID {
    expr *temp;

    temp = check_field_access(&($1), $3);
    $$ = *temp;
    free(temp);
 }
    | unary '(' prepare_function_call args ')' {
    expr *temp;

    temp = check_function_call(&($1), $4);
    $$ = *temp;
    print_code("%s:\n", current_return_label()->name);
    exit_branch();
    free(temp);
 }
    | unary '(' prepare_function_call ')' {
    expr *temp;

    temp = check_function_call(&($1), NULL);
    $$ = *temp;
    print_code("%s:\n", current_return_label()->name);
    exit_branch();
    free(temp);
 }
    ;
prepare_function_call: /* empty */ {
    /* push hole, push return address, push prev frame pointer, update frame pointer */
    expr *temp;
    function *fun;

    temp = &($<expr>-1);
    enter_branch(0);
    if (temp->ctor == Id && temp->value.id_val->val->ctor == Fun) {
        fun = find_function(temp->value.id_val->key);
        if (fun) {
            print_code("\tshift_sp %d\n", type_size(fun->ret));
            print_code("\tpush_const %s\n", current_return_label()->name);
            print_code("\tpush_reg fp\n");
            print_code("\tpush_reg sp\n");
            print_code("\tpop_reg fp\n");
            enter_scope();
        }
    }
 }
   ;
args: expr {
    arg_list *list;
    list = malloc(sizeof(arg_list));
    list->arg = $1;
    list->next = NULL;

    $$ = list;
 }
    | args ',' expr {
    arg_list *list;
    list = malloc(sizeof(arg_list));
    list->arg = $3;
    list->next = $1;

    $$ = list;
 }
    ;
%%

/*  Additional C Codes  */

int yyerror (char* s)
{
    print_error(s);
    return 0;
}
