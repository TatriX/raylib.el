.POSIX:

all: raylib.el.so raylib.elc

raylib.el.so: raylib.c
	gcc -fPIC -g -Wall raylib.c -shared -o raylib.el.so -lraylib

raylib.elc: raylib.el
	emacs -Q --batch -L . -f batch-byte-compile raylib.el

test:
	emacs -Q --module-assertions --load test.el

clean:
	rm raylib.el.so raylib.elc
