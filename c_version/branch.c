#include "subc.h"
#include "subc.tab.h"

static int num_of_branches = 0;
static branch_stack *branches = NULL;

void enter_branch(int is_loop)
{
	static char label_name[30];
	id *if_label, *else_label, *return_label;
	branch_stack *node;

	sprintf(label_name, "If%x", num_of_branches);
	if_label = enter(ID, label_name, strlen(label_name));

	sprintf(label_name, "Else%x", num_of_branches);
	else_label = enter(ID, label_name, strlen(label_name));

	sprintf(label_name, "Return%x", num_of_branches);
	return_label = enter(ID, label_name, strlen(label_name));

	++num_of_branches;
	node = malloc(sizeof(branch_stack));
	node->loop = is_loop;
	node->depth = current_depth();
	node->if_label = if_label;
	node->else_label = else_label;
	node->return_label = return_label;
	node->next = branches;

	branches = node;
}

void exit_branch(void)
{
	branch_stack *node;
	node = branches;
	branches = node->next;
	free(node);
}

branch_stack *current_loop(void)
{
	branch_stack **top;
	branch_stack *ret;

	ret = NULL;
	for (top = &branches; *top; top = &(*top)->next) {
		if ((*top)->loop) {
			ret = *top;
			break;
		}
	}

	return ret;
}

id *current_if_label(void)
{
	if (branches)
		return branches->if_label;
	else
		return NULL;
}

id *current_else_label(void)
{
	if (branches)
		return branches->else_label;
	else
		return NULL;
}

id *current_return_label(void)
{
	if (branches)
		return branches->return_label;
	else
		return NULL;
}
