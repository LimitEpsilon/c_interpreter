/***************************************************************
 * File Name    : hash.c
 * Description
 *      This is an implementation file for the open hash table.
 *
 ****************************************************************/

#include "subc.h"
#include "subc.tab.h"
#include <string.h>

#define HASH_TABLE_SIZE 101

static node *hashTable[HASH_TABLE_SIZE];

void failwith(const char *msg)
{
	fprintf(stderr, "%s\n", msg);
	exit(1);
}

unsigned hash(char *name)
{
	unsigned h;
	h = 0;

	for (; *name != '\0'; ++name) {
		h <<= 1;
		h += *name;
		h %= HASH_TABLE_SIZE;
	}

	return h;
}

WHAT_CHILD_AM_I(node)

ROTATE(node)

BALANCE (node)

static id *find(node *root, char *name)
{
	id *found;
	int cmp;
	found = 0;

	while (root != 0) {
		if ((cmp = strcmp(name, root->data->name)) < 0)
			root = root->children[Left];
		else if (cmp > 0)
			root = root->children[Right];
		else {
			found = root->data;
			break;
		}
	}

	return found;
}

static void insert(node **bucket, id *data)
{
	node *n, *root, *prev;
	direction dir;

	n = malloc(sizeof(node));
	if (!n)
		failwith(strerror(errno));
	n->data = data;
	n->children[Left] = n->children[Right] = 0;
	n->color = Red;

	root = *bucket;
	prev = 0;

	while (root != 0) {
		prev = root;
		if (strcmp(data->name, root->data->name) < 0)
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

id *enter(int tokenType, char *name, int length)
{
	id *temp_id;
	node **bucket;
	char *temp_name;
	int h;

	h = hash(name);
	bucket = hashTable + h;
	temp_id = find(*bucket, name);

	if (temp_id) /* if in the hashtbl, return */
		return temp_id;

	/* if not in the hashtable, copy to new memory, insert, return */
	temp_name = malloc(sizeof(char) * (length + 1));
	if (!temp_name)
		failwith(strerror(errno));
	strcpy(temp_name, name);

	temp_id = malloc(sizeof(id));
	if (!temp_id)
		failwith(strerror(errno));
	temp_id->tokenType = tokenType;
	temp_id->name = temp_name;

	switch (tokenType) {
	case TYPE:
	case STRUCT:
	case NULLPTR:
	case IF:
	case ELSE:
	case WHILE:
	case FOR:
	case RETURN:
	case BREAK:
	case CONTINUE: /* keywords */
	case STRING: /* starts with a " */
	case RELOP:
	case EQUOP: /* other tokens with stringVals */
		insert(bucket, temp_id);
		break;
	default: /* ID */
		insert(bucket, temp_id);
		break;
	}

	return temp_id;
}

/********************* for debugging purposes *********************/

void print_rbtree(node *root, int depth)
{
	int i;

	if (!root)
		return;

	if (root->color == Red && root->parent->color == Red) {
		printf("rbtree violated\n");
		exit(1);
	}

	for (i = 0; i < depth; ++i)
		printf("\t");
	printf("%s[%d]\n", root->data->name, root->color);

	print_rbtree(root->children[Left], depth + 1);
	print_rbtree(root->children[Right], depth + 1);
	return;
}

void print_table(void)
{
	int i;
	for (i = 0; i < HASH_TABLE_SIZE; ++i) {
		printf("Bucket number %d\n", i);
		print_rbtree(hashTable[i], 0);
	}
	return;
}
