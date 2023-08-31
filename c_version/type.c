#include "subc.h"

type_node *types = NULL;

WHAT_CHILD_AM_I(type_node)

ROTATE(type_node)

BALANCE(type_node)

static int compare(type *t1, type *t2)
{
	int ret;

	if (t1->ctor < t2->ctor)
		ret = -1;
	else if (t1->ctor > t2->ctor)
		ret = 1;
	else {
		switch (t1->ctor) {
		case Int:
		case Char:
		case Void:
			ret = 0;
			break;
		case Ptr:
			if (t1->arg.ptr < t2->arg.ptr)
				ret = -1;
			else if (t1->arg.ptr > t2->arg.ptr)
				ret = 1;
			else
				ret = 0;
			break;
		case Struct:
			if (t1->arg.struct_name < t2->arg.struct_name)
				ret = -1;
			else if (t1->arg.struct_name > t2->arg.struct_name)
				ret = 1;
			else
				ret = 0;
			break;
		}
	}

	return ret;
}

static type *find(type_node *root, type *search)
{
	type *found;
	int compared;
	found = NULL;

	while (root != NULL) {
		compared = compare(search, root->type);
		if (compared < 0)
			root = root->children[Left];
		else if (compared > 0)
			root = root->children[Right];
		else {
			found = root->type;
			break;
		}
	}

	return found;
}

static void insert(type_node **bucket, type *type)
{
	type_node *n, *root, *prev;
	direction dir;

	n = malloc(sizeof(type_node));
	if (!n)
		failwith(strerror(errno));
	n->type = type;
	n->children[Left] = n->children[Right] = 0;
	n->color = Red;

	root = *bucket;
	prev = 0;

	while (root != 0) {
		prev = root;
		if (compare(type, root->type) < 0)
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

type *enter_type(type_ctor ctor, type *ptr, id *struct_name)
{
	type *temp, *ret;
	temp = malloc(sizeof(type));

	if (!temp)
		failwith("Out of memory");

	temp->ctor = ctor;
	switch (ctor) {
	case Ptr:
		temp->arg.ptr = ptr;
		break;
	case Struct:
		temp->arg.struct_name = struct_name;
		break;
	default:
		break;
	}

	ret = find(types, temp);
	if (ret)
		free(temp);
	else {
		insert(&types, temp);
		ret = temp;
	}

	return ret;
}

int type_size(type *ty)
{
	int ret;
	tbl_val *struct_type;
	env *struct_fields;

	if (!ty || ty->ctor == Void)
		ret = 0;
	else if (ty->ctor == Struct) {
		struct_type = find_struct(ty->arg.struct_name);
		if (struct_type && struct_type->env) {
			struct_fields = struct_type->env;
			ret = struct_fields->val->offset +
			      struct_fields->val->size;
		} else
			ret = 0;
	} else
		ret = 1;

	return ret;
}
