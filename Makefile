.POSIX:
EMACS = emacs
CC = gcc

all: raylib.el.so raylib.elc

raylib.el.so: raylib.c
	$(CC) -fPIC -g -Wall -c raylib.c
	$(CC) -shared -o raylib.el.so raylib.o -lraylib

raylib.elc: raylib.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile raylib.el

test:
	emacs -Q --module-assertions --load test.el

clean:
	rm raylib.el.so raylib.elc
