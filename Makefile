.POSIX:
EMACS = emacs
CFLAGS = -g -Wall # TODO: build with optimisations!
LDFLAGS = -s

all: raylib.el.so raylib.elc

raylib.el.so: raylib.c
	cc -g -shared -fPIC $(CFLAGS) $(LDFLAGS) -o raylib.el.so raylib.c -lraylib

raylib.elc: raylib.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile raylib.el

test:
	emacs -Q --module-assertions --load test.el

clean:
	rm raylib.el.so raylib.elc
