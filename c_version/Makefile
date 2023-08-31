#
# This is a makefile for the skeleton program for project 2.
#

CC = gcc
LEX = flex
YACC = bison
COMPILE_OPTION = -g -Wall
LINK_OPTION = -lfl
YACC_OPTION = -vd

all : subc

clean :
	rm -f *.o lex.yy.c subc.tab.c subc.tab.h subc.output subc

subc : lex.yy.o hash.o branch.o type.o env.o tbl.o type_checker.o subc.tab.o
	${CC} -o subc lex.yy.o hash.o branch.o type.o env.o tbl.o type_checker.o subc.tab.o ${LINK_OPTION}

subc.tab.o : subc.tab.c subc.h
	${CC} -c ${COMPILE_OPTION} subc.tab.c

hash.o : hash.c subc.h rbtree.h
	${CC} -c ${COMPILE_OPTION} hash.c

branch.o : branch.c subc.h
	${CC} -c ${COMPILE_OPTION} branch.c

type.o : type.c subc.h rbtree.h
	${CC} -c ${COMPILE_OPTION} type.c

env.o : env.c subc.h rbtree.h
	${CC} -c ${COMPILE_OPTION} env.c

tbl.o : tbl.c subc.h rbtree.h
	${CC} -c ${COMPILE_OPTION} tbl.c

type_checker.o : type_checker.c subc.h
	${CC} -c ${COMPILE_OPTION} type_checker.c

lex.yy.o : lex.yy.c subc.tab.h subc.h
	${CC} -c ${COMPILE_OPTION} lex.yy.c

lex.yy.c : subc.l
	${LEX} subc.l

subc.tab.h subc.tab.c : subc.y
	${YACC} ${YACC_OPTION} subc.y

