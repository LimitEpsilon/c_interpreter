#include "subc.h"

static env_stack *envs = NULL;
static int env_stack_depth = -1; /* for the global scope to have depth 0 */

static env *find(env *root, id *name)
{
	while (root != NULL) {
		if (name != root->key)
			root = root->next;
		else
			break;
	}

	return root;
}

static void insert(env **bucket, id *key, env_val *val)
{
	env *n;
	n = malloc(sizeof(env));
	if (!n)
		failwith(strerror(errno));

	n->key = key;
	n->val = val;
	n->next = *bucket;

	if (*bucket)
		val->offset = (*bucket)->val->offset + (*bucket)->val->size;
	else
		val->offset = 0;

	*bucket = n;
	return;
}

int top_data_size(void)
{
	return (envs->top ? (envs->top->val->offset + envs->top->val->size)
		          : 0);
}

int current_depth(void)
{
	return env_stack_depth;
}

void free_scope(env *e)
{
	env *temp;

	while (e) {
		temp = e->next;
		free(e->val);
		free(e);
		e = temp;
	}

	return;
}

env *exit_scope(void)
{
	env_stack *stack_top;
	env *ret;

	--env_stack_depth;
	stack_top = envs;
	envs = stack_top->next;
	ret = stack_top->top;

	free(stack_top);
	return ret;
}

void enter_scope(void)
{
	env_stack *stack_top;

	++env_stack_depth;
	stack_top = malloc(sizeof(env_stack));
	stack_top->top = NULL;
	stack_top->next = envs;

	envs = stack_top;
	return;
}

void enter_var(id *name, type *ty, int ptrs)
{
	int make_ptr;

	if (!ty)
		return;

	for (make_ptr = 0; make_ptr < ptrs; ++make_ptr) {
		ty = enter_type(Ptr, ty, NULL);
	}

	update_env(Var, name, ty, type_size(ty));
}

void enter_arr(id *name, type *ty, int ptrs, int size)
{
	int make_ptr;

	if (!ty)
		return;

	for (make_ptr = 0; make_ptr < ptrs; ++make_ptr) {
		ty = enter_type(Ptr, ty, NULL);
	}

	update_env(Arr, name, ty, type_size(ty) * size);
}

void enter_fun(id *name)
{
	update_env(Fun, name, NULL, 0);
}

int update_env(env_ctor ctor, id *name, type *ty, int size)
{
	env **top, *found;
	env_val *to_insert;

	top = &envs->top;
	found = find(*top, name);
	if (found) {
		print_error("redeclaration");
		return 0;
	}

	to_insert = malloc(sizeof(env_val));
	to_insert->ctor = ctor;
	to_insert->type = ty;
	to_insert->size = size;
	to_insert->depth = env_stack_depth;
	insert(top, name, to_insert);
	return 1;
}

env *find_var(id *name)
{
	env_stack **top;
	env *ret;

	ret = NULL;
	for (top = &envs; *top; top = &(*top)->next) {
		ret = find((*top)->top, name);
		if (!ret)
			continue;
		else
			break;
	}

	if (!ret)
		print_error("not declared");

	return ret;
}

env *find_field(env *env, id *field_name)
{
	return find(env, field_name);
}
