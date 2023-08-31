/******************************************************
 * File Name   : subc.h
 * Description
 *    This is a header file for the subc program.
 ******************************************************/

#ifndef __SUBC_H__
#define __SUBC_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "rbtree.h"

void turn_on_arr(void);
void turn_off_arr(void);
void print_error(const char *msg);
void print_code(const char *fmt, ...);

typedef enum { False = 0, True } boolean;

/******************** hash.c ********************/
/* Name table: string -> id */
typedef struct id {
	int tokenType;
	char *name;
} id;

typedef struct node {
	struct node *parent;
	struct node *children[2];
	color color;
	id *data;
} node;

/* For the name table */
id *enter(int tokenType, char *name, int length);

/***************** Type environments *****************/

/******************** type.c ********************/
/* Types are stored in a "master" rbtree and are never freed */
/* Comparison between types are performed by comparing type ptrs */
struct type; /* forward declaration */

typedef enum { Int, Char, Void, Struct, Ptr } type_ctor;

typedef union {
	struct type *ptr;
	id *struct_name;
} type_arg;

typedef struct type {
	type_ctor ctor;
	type_arg arg;
} type;

typedef struct type_node {
	struct type_node *parent;
	struct type_node *children[2];
	color color;
	type *type;
} type_node;

type *enter_type(type_ctor ctor, type *ptr, id *struct_name);
int type_size(type *ty);

/******************* env.c *******************/
/* env: id -> Var of type | Arr of type * size | Fun */
/* Types of functions are figured out by consulting the function_tbl */
typedef enum { Var, Fun, Arr } env_ctor;

typedef struct env_val {
	env_ctor ctor;
	type *type;
	int size;
	int offset;
	int depth; /* how deep is my scope? 0: global, pos: local */
} env_val;

typedef struct env {
	id *key;
	env_val *val;
	struct env *next;
} env;

/* stack of environments */
typedef struct env_stack {
	env *top;
	struct env_stack *next;
} env_stack;

int current_depth(void);
int top_data_size(void);
void enter_scope(void);
void enter_var(id *name, type *ty, int ptrs);
void enter_arr(id *name, type *ty, int ptrs, int size);
void enter_fun(id *name);
env *exit_scope(void);
void free_scope(env *e);

/* do not use in subc.y */
int update_env(env_ctor ctor, id *name, type *ty, int size);
env *find_var(id *name);
env *find_field(env *env, id *field_name);

/******************* tbl.c *******************/
/* function = return type * parameter type */
/* function_tbl: id -> function type */
typedef struct function {
	type *ret;
	env *params;
	boolean defined;
} function;

/* definition of common data structure for tables with struct id ptrs as keys */
typedef union {
	function *function; /* for function_tbl */
	env *env; /* for struct_tbl */
} tbl_val;

typedef struct tbl {
	struct tbl *parent;
	struct tbl *children[2];
	color color;
	id *key;
	tbl_val *val;
} tbl;

/* allocate new struct function with defined = False */
function *make_function(type *ret, int ptrs, env *params);
/* returns NULL if double declaration is detected */
int insert_function(id *name, function *insert_fun);
/* returns NULL if the function is not declared yet */
function *find_function(id *name);
/* returns 0 if double declaration is detected */
int insert_struct(id *name, env *fields);
/* returns NULL if the struct is not defined yet */
tbl_val *find_struct(id *name);

/***************** Exprs and Stmts *****************/

/* expressions must start from its roots, the unary exprs */
/* the constructors give info about what is on the top of the stack machine */
/* Lvalues and Ids have their locations on the top of the stack after reduction */
typedef enum {
	Id, /* functions, variables, struct fields */
	Const_int,
	Const_char,
	Const_string,
	Lvalue, /* arr[i], *ptr */
	Rvalue,
	Error /* type error */
} expr_ctor;

typedef union {
	int int_val;
	char char_val;
	id *string_val;
	env *id_val;
	type *lr_val;
} expr_val;

/* expressions */
typedef struct expr {
	expr_ctor ctor;
	expr_val value;
} expr;

typedef struct arg_list {
	expr arg;
	struct arg_list *next;
} arg_list;

/******************* branch.c *******************/
/* label for commands */
typedef struct branch_stack {
	int loop; /* is this branch from a loop? */
	int depth; /* what is the depth that the branch started? */
	id *if_label;
	id *else_label;
	id *return_label;
	struct branch_stack *next;
} branch_stack;

void enter_branch(int is_loop);
void exit_branch(void);
branch_stack *current_loop(void);
id *current_if_label(void);
id *current_else_label(void);
id *current_return_label(void);

/******************* type_checker.c *******************/
/* For type checking */
typedef enum {
	Pre_incr,
	Pre_decr,
	Post_incr,
	Post_decr,
	Ref,
	Deref,
	Neg,
	Not
} unary_op;

typedef enum {
	Assign,
	Or,
	And,
	Lt,
	Gt,
	Le,
	Ge,
	Eq,
	Neq,
	Add,
	Sub,
	Arr_access
} binary_op;

void turn_on_structop(void);
void turn_off_structop(void);
type *what_type(expr *e);
expr *check_unary_op(unary_op op, expr *e);
expr *check_binary_op(binary_op op, expr *e1, expr *e2);
expr *check_function_call(expr *f, arg_list *args);
expr *check_field_access(expr *str, id *fld);

/* for panic */
void failwith(const char *msg);
#endif
