#include "subc.h"
#include "subc.tab.h"

static int is_structop = 0;

void turn_on_structop(void)
{
	is_structop = 1;
}

void turn_off_structop(void)
{
	is_structop = 0;
}

type *what_type(expr *e)
{
	type *ret;

	switch (e->ctor) {
	case Id:
		if (e->value.id_val->val->ctor == Var)
			ret = e->value.id_val->val->type;
		else
			ret = NULL;
		break;
	case Const_int:
		ret = enter_type(Int, NULL, NULL);
		break;
	case Const_char:
		ret = enter_type(Char, NULL, NULL);
		break;
	case Const_string:
		ret = enter_type(Ptr, enter_type(Char, NULL, NULL), NULL);
		break;
	case Lvalue:
	case Rvalue:
		ret = e->value.lr_val;
		break;
	default:
		ret = NULL;
		break;
	}

	return ret;
}

expr *check_unary_op(unary_op op, expr *e)
{
	expr *ret;
	type *t;

	ret = malloc(sizeof(expr));

	t = what_type(e);
	if (!t) {
		ret->ctor = Error;
		goto escape;
	}

	switch (op) {
	case Pre_incr:
		print_code("\tpush_reg sp\n");
		print_code("\tfetch\n");
		print_code("\tpush_reg sp\n");
		print_code("\tfetch\n");
		print_code("\tfetch\n");
		print_code("\tpush_const 1\n");
		print_code("\tadd\n");
		print_code("\tassign\n");
		print_code("\tfetch\n");
		goto check_incr_decr;
	case Pre_decr:
		print_code("\tpush_reg sp\n");
		print_code("\tfetch\n");
		print_code("\tpush_reg sp\n");
		print_code("\tfetch\n");
		print_code("\tfetch\n");
		print_code("\tpush_const 1\n");
		print_code("\tsub\n");
		print_code("\tassign\n");
		print_code("\tfetch\n");
		goto check_incr_decr;
	case Post_incr:
		print_code("\tpush_reg sp\n");
		print_code("\tfetch\n");
		print_code("\tpush_reg sp\n");
		print_code("\tfetch\n");
		print_code("\tfetch\n");
		print_code("\tpush_const 1\n");
		print_code("\tadd\n");
		print_code("\tassign\n");
		print_code("\tfetch\n");
		print_code("\tpush_const 1\n");
		print_code("\tsub\n");
		goto check_incr_decr;
	case Post_decr:
		print_code("\tpush_reg sp\n");
		print_code("\tfetch\n");
		print_code("\tpush_reg sp\n");
		print_code("\tfetch\n");
		print_code("\tfetch\n");
		print_code("\tpush_const 1\n");
		print_code("\tsub\n");
		print_code("\tassign\n");
		print_code("\tfetch\n");
		print_code("\tpush_const 1\n");
		print_code("\tadd\n");
check_incr_decr:
		ret->ctor = Rvalue;
		switch (e->ctor) {
		case Id:
		case Lvalue:
			if (t->ctor == Int)
				ret->value.lr_val = t;
			else if (t->ctor == Ptr)
				ret->value.lr_val = t;
			else
				ret->ctor = Error;
			break;
		default:
			ret->ctor = Error;
			break;
		}
		break;
	case Ref:
		ret->ctor = Rvalue;
		switch (e->ctor) {
		case Id:
		case Lvalue:
			ret->value.lr_val = enter_type(Ptr, t, NULL);
			break;
		default:
			ret->ctor = Error;
			break;
		}
		break;
	case Deref:
		ret->ctor = Lvalue;
		switch (e->ctor) {
		case Id:
		case Lvalue:
			print_code("\tfetch\n");
		case Rvalue:
			if (t->ctor == Ptr)
				ret->value.lr_val = t->arg.ptr;
			else
				ret->ctor = Error;
			break;
		default:
			ret->ctor = Error;
			break;
		}
		break;
	case Neg:
		if (e->ctor == Id || e->ctor == Lvalue) {
			print_code("\tfetch\n");
		}
		print_code("\tnegate\n");
		goto check_not;
	case Not:
		if (e->ctor == Id || e->ctor == Lvalue) {
			print_code("\tfetch\n");
		}
		print_code("\tnot\n");
check_not:
		ret->ctor = Rvalue;
		if (t->ctor == Int)
			ret->value.lr_val = enter_type(Int, NULL, NULL);
		else
			ret->ctor = Error;
		break;
	}

escape:
	if (ret->ctor == Error) {
		switch (op) {
		case Ref:
			print_error("not a variable");
			break;
		case Deref:
			if (is_structop)
				print_error("not a struct pointer");
			else
				print_error("not a pointer");
			break;
		default:
			print_error("not computable");
			break;
		}
	}
	return ret;
}

expr *check_binary_op(binary_op op, expr *e1, expr *e2)
{
	expr *ret;
	type *t1, *t2;

	ret = malloc(sizeof(expr));
	if (e1->ctor == Error || e2->ctor == Error) {
		ret->ctor = Error;
		goto escape;
	}

	t1 = what_type(e1);
	t2 = what_type(e2);

	switch (op) {
	case Assign: /* are the two types equal? */
		ret->ctor = Rvalue;
		if (t1 && t2 && t1 == t2)
			ret->value.lr_val = t1;
		else if (t2 && t2->ctor == Ptr && !t2->arg.ptr) { /* nullptr */
			if (t1 && t1->ctor == Ptr)
				ret->value.lr_val = t1;
			else {
				ret->ctor = Error;
				print_error("RHS is not a const or variable");
			}
		} else
			ret->ctor = Error;
		break;
	case Or:
		print_code("\tor\n");
		goto check_logic;
	case And:
		print_code("\tand\n");
check_logic:
		ret->ctor = Rvalue;
		if (t1 && t2)
			ret->value.lr_val = enter_type(Int, NULL, NULL);
		else
			ret->ctor = Error;
		break;
	case Add:
		print_code("\tadd\n");
		goto check_arithmetic;
	case Sub:
		print_code("\tsub\n");
check_arithmetic:
		ret->ctor = Rvalue;
		if (t1 && t2 &&
		    ((t1->ctor == Int && t2->ctor == Int) ||
		     (t1->ctor == Ptr && t2->ctor == Int)))
			ret->value.lr_val = t1;
		else
			ret->ctor = Error;
		break;
	case Ge:
		print_code("\tgreater_equal\n");
		goto check_comparison;
	case Le:
		print_code("\tless_equal\n");
		goto check_comparison;
	case Gt:
		print_code("\tgreater\n");
		goto check_comparison;
	case Lt:
		print_code("\tless\n");
check_comparison:
		ret->ctor = Rvalue;
		if (t1 && t2 &&
		    ((t1->ctor == Int && t2->ctor == Int) ||
		     (t1->ctor == Char && t2->ctor == Char)))
			ret->value.lr_val = enter_type(Int, NULL, NULL);
		else
			ret->ctor = Error;
		break;
	case Neq:
		print_code("\tnot_equal\n");
		goto check_equality;
	case Eq:
		print_code("\tequal\n");
check_equality:
		ret->ctor = Rvalue;
		if (t1 && t2 &&
		    ((t1->ctor == Int && t2->ctor == Int) || /* both ints */
		     (t1->ctor == Char && t2->ctor == Char) || /* both chars */
		     (t1->ctor == Ptr && t2->ctor == Ptr && /* both ptrs */
		      (!t1->arg.ptr || !t2->arg.ptr || t1 == t2))))
			ret->value.lr_val = enter_type(Int, NULL, NULL);
		else
			ret->ctor = Error;
		break;
	case Arr_access:
		ret->ctor = Lvalue;
		if (e1->ctor == Id && e1->value.id_val->val->ctor == Arr &&
		    t2 && t2->ctor == Int) {
			ret->value.lr_val = e1->value.id_val->val->type;
			print_code("\tpush_const %d\n",
				   type_size(ret->value.lr_val));
			print_code("\tmul\n");
			print_code("\tadd\n");
		} else
			ret->ctor = Error;
		break;
	}

escape:
	if (ret->ctor == Error) {
		switch (op) {
		case Gt:
		case Lt:
		case Ge:
		case Le:
		case Neq:
		case Eq:
			print_error("not comparable");
			break;
		case Arr_access:
			print_error("not an array type");
			break;
		case Assign:
			print_error("LHS and RHS are not same type");
			break;
		default:
			print_error("not computable");
			break;
		}
	}
	return ret;
}

expr *check_function_call(expr *f, arg_list *args)
{
	expr *ret;
	arg_list *head;
	function *fun;
	id *write_int, *write_string, *write_char, *fun_name;
	env *cursor;
	type *t;

	head = args; /* save the head */
	ret = malloc(sizeof(expr));
	if (!ret)
		failwith("Out of memory");

	ret->ctor = Rvalue;

	if (f->ctor != Id || f->value.id_val->val->ctor != Fun) {
		print_error("not a function");
		ret->ctor = Error;
		goto escape;
	}

	fun_name = f->value.id_val->key;
	write_int = enter(ID, "write_int", 9);
	write_string = enter(ID, "write_string", 12);
	write_char = enter(ID, "write_char", 10);

	if (fun_name == write_int) {
		print_code("\twrite_int\n");
		ret->value.lr_val = enter_type(Void, NULL, NULL);
		goto escape;
	} else if (fun_name == write_string) {
		print_code("\twrite_string\n");
		ret->value.lr_val = enter_type(Void, NULL, NULL);
		goto escape;
	} else if (fun_name == write_char) {
		print_code("\twrite_char\n");
		ret->value.lr_val = enter_type(Void, NULL, NULL);
		goto escape;
	}

	fun = find_function(fun_name);
	if (!fun) {
		ret->ctor = Error;
		goto escape;
	}

	free_scope(exit_scope());
	ret->value.lr_val = fun->ret;

	for (cursor = fun->params; cursor; cursor = cursor->next) {
		if (!args) {
			ret->ctor = Error;
			goto escape;
		}

		t = what_type(&args->arg);
		if (cursor->val->ctor == Arr && args->arg.ctor == Id &&
		    args->arg.value.id_val->val->ctor == Arr &&
		    cursor->val->size == args->arg.value.id_val->val->size)
			args = args->next;
		else if (cursor->val->type == t)
			args = args->next;
		else if (cursor->val->type->ctor == Ptr && t &&
			 t->ctor == Ptr && !t->arg.ptr)
			args = args->next;
		else {
			ret->ctor = Error;
			goto escape;
		}
	}

	if (args)
		ret->ctor = Error;
	else
		print_code("\tjump %s\n", fun_name->name);

escape:
	/* free arg_list */
	for (args = head; args; args = head) {
		head = head->next;
		free(args);
	}
	/* print error message */
	if (ret->ctor == Error)
		print_error("actual args are not equal to formal args");
	return ret;
}

expr *check_field_access(expr *str, id *fld)
{
	expr *ret;
	type *t;
	env *struct_env, *var;

	ret = malloc(sizeof(expr));

	t = what_type(str);

	if (t && t->ctor == Struct) {
		if (str->ctor == Rvalue) {
			print_error(
				"does not support struct access from a rvalue");
			ret->ctor = Error;
			goto escape;
		}
		struct_env = find_struct(t->arg.struct_name)->env;
		if (!struct_env) {
			print_error("struct not have same name field");
			ret->ctor = Error;
			goto escape;
		}
		var = find_field(struct_env, fld);
		if (!var) {
			print_error("struct not have same name field");
			ret->ctor = Error;
			goto escape;
		}
		ret->ctor = Id;
		ret->value.id_val = var;
		print_code("\tpush_const %d\n", var->val->offset);
		print_code("\tadd\n");
	} else {
		if (is_structop)
			print_error("not a struct pointer");
		else
			print_error("not a struct");
		ret->ctor = Error;
	}

escape:
	return ret;
}
