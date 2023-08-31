#include "subc.h"
#include "subc.tab.h"

static tbl *functions = NULL;
static tbl *structs = NULL;

WHAT_CHILD_AM_I(tbl)

ROTATE(tbl)

BALANCE(tbl)

static tbl_val *find(tbl *root, id *name)
{
	tbl_val *found;
	found = NULL;

	while (root != NULL) {
		if (name < root->key)
			root = root->children[Left];
		else if (name > root->key)
			root = root->children[Right];
		else {
			found = root->val;
			break;
		}
	}

	return found;
}

static void insert(tbl **bucket, id *key, tbl_val *val)
{
	tbl *n, *root, *prev;
	direction dir;

	n = malloc(sizeof(tbl));
	if (!n)
		failwith(strerror(errno));
	n->key = key;
	n->val = val;
	n->children[Left] = n->children[Right] = 0;
	n->color = Red;

	root = *bucket;
	prev = 0;

	while (root != 0) {
		prev = root;
		if (key < root->key)
			dir = Left;
		else
			dir = Right;
		root = root->children[dir];
	}

	n->parent = prev;
	if (prev == 0) {
		*bucket = n;
		n->color = Black;
		return;
	} else
		prev->children[dir] = n;

	balance(bucket, n);
	return;
}

static void free_function(function *fun)
{
	free_scope(fun->params);
	free(fun);
}

static int check_params(env *before, env *after)
{
	for (; before && after; before = before->next, after = after->next)
		if (before->val->ctor == after->val->ctor &&
		    before->val->type != after->val->type)
			return 0;

	if (!before && !after)
		return 1;
	else /* different length */
		return 0;
}

function *make_function(type *ret, int ptrs, env *params)
{
	int make_ptr;
	function *fun;

	if (!ret)
		return NULL;

	for (make_ptr = 0; make_ptr < ptrs; ++make_ptr) {
		ret = enter_type(Ptr, ret, NULL);
	}

	fun = malloc(sizeof(function));
	if (!fun)
		failwith("Out of memory");

	fun->ret = ret;
	fun->params = params;
	fun->defined = False;

	return fun;
}

int insert_function(id *name, function *insert_fun)
{
	tbl **top;
	tbl_val *found;

	if (!insert_fun)
		return 0;

	top = &functions;
	found = find(*top, name);
	if (found) {
		if (found->function->defined == True &&
		    insert_fun->defined == True) {
			free_function(insert_fun);
			return 0;
		} else {
			if (found->function->ret == insert_fun->ret &&
			    check_params(found->function->params,
					 insert_fun->params)) {
				if (insert_fun->defined == True) {
					free_function(found->function);
					found->function = insert_fun;
				} else
					free_function(insert_fun);
				return 1;
			} else {
				if (insert_fun->defined == False)
					free_function(insert_fun);
				print_error("function types do not match with previous declaration");
				return 0;
			}
		}
	}

	found = malloc(sizeof(tbl_val));
	if (!found)
		failwith("Out of memory");
	found->function = insert_fun;

	insert(top, name, found);
	return 1;
}

function *find_function(id *name)
{
	tbl_val *found;
	function *ret;

	ret = NULL;
	found = find(functions, name);
	if (found)
		ret = found->function;

	return ret;
}

/* always called after consulting find_struct */
int insert_struct(id *name, env *fields)
{
	tbl_val *new_struct;

	new_struct = malloc(sizeof(tbl_val));
	if (!new_struct)
		failwith("Out of memory");

	new_struct->env = fields;
	insert(&structs, name, new_struct);
	return 1;
}

tbl_val *find_struct(id *name)
{
	return find(structs, name);
}
