 CFLAGS=-g

# TODO: build with optimisations!
# CFLAGS=-o2
all:
	gcc $(CFLAGS) -Wall -c -fPIC raylib.c
	gcc -shared -o raylib.so raylib.o -lraylib

test:
	emacs -Q --module-assertions --load test.el
