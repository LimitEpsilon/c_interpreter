#ifndef __RBTREE_H__
#define __RBTREE_H__

/* We use rbtrees to implement a map of keys to values
 * This header file provides the color and direction types
 * and macros to implement the generic rotate, balance functions */

/* rbtree: 'a -> 'b */
typedef enum { Red, Black } color;

/* direction is hard-coded, please do not change the order of def */
typedef enum { Left = 0, Right, Root } direction;

#define WHAT_CHILD_AM_I(nodetype)                        \
	static direction what_child_am_i(nodetype *n)    \
	{                                                \
		if (n->parent == 0)                      \
			return Root;                     \
		else if (n->parent->children[Left] == n) \
			return Left;                     \
		else                                     \
			return Right;                    \
	}

#define ROTATE(nodetype)                                                       \
	static void rotate(direction dir, nodetype **bucket, nodetype *axis)   \
	{                                                                      \
		nodetype *rotated; /* the node to be rotated about the axis */ \
		rotated = axis->children[1 - dir]; /* must be non-null */      \
		/* rotation affects four nodes                             */  \
		/* the axis, the rotated node,                             */  \
		/* the child of the rotated node in the rotated direction, */  \
		/* and the parent of the axis                              */  \
                                                                               \
		/* connect the axis with the child of the rotated node */      \
		axis->children[1 - dir] = rotated->children[dir];              \
		if (rotated->children[dir] != 0)                               \
			rotated->children[dir]->parent = axis;                 \
                                                                               \
		/* connect the parent of the axis with the rotated node */     \
		rotated->parent = axis->parent;                                \
		switch (what_child_am_i(axis)) {                               \
		case Root:                                                     \
			*bucket = rotated;                                     \
			break;                                                 \
		case Left:                                                     \
			axis->parent->children[Left] = rotated;                \
			break;                                                 \
		case Right:                                                    \
			axis->parent->children[Right] = rotated;               \
			break;                                                 \
		}                                                              \
                                                                               \
		/* connect the axis node with the rotated node */              \
		rotated->children[dir] = axis;                                 \
		axis->parent = rotated;                                        \
		return;                                                        \
	}

#define BALANCE(nodetype)                                                                       \
	static void balance(nodetype **bucket, nodetype *n)                                     \
	{                                                                                       \
		nodetype *this_side;                                                            \
		nodetype *other_side;                                                           \
		direction dir;                                                                  \
                                                                                                \
		this_side = n->parent; /* invariant */                                          \
                                                                                                \
		/* invariant : this_side is not root */                                         \
		while (this_side != 0 && this_side->color == Red) {                             \
			dir = what_child_am_i(this_side);                                       \
			other_side = this_side->parent->children[1 - dir];                      \
                                                                                                \
			if (other_side != 0 && other_side->color == Red) {                      \
				this_side->color = Black;                                       \
				other_side->color = Black;                                      \
				this_side->parent->color = Red;                                 \
                                                                                                \
				n = this_side->parent;                                          \
				this_side = n->parent;                                          \
			} else {                                                                \
				if (what_child_am_i(n) != dir) {                                \
					n = this_side;                                          \
					rotate(dir, bucket, n);                                 \
					this_side =                                             \
						n->parent; /* rotation changes node's parent */ \
				}                                                               \
				this_side->color = Black;                                       \
				this_side->parent->color = Red;                                 \
				rotate(1 - dir, bucket, this_side->parent);                     \
			}                                                                       \
		}                                                                               \
                                                                                                \
		(*bucket)->color = Black;                                                       \
		return;                                                                         \
	}

/* need to implement find, insert for each node type */

#endif
