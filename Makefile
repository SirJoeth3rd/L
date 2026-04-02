CC = gcc
CFLAGS = -I./tinycc/include
LDFLAGS = -L./tinycc -ltcc -ldl -lm

build:
	$(CC) $(CFLAGS) L.c -g -std=c99 $(LDFLAGS)

test: build
	./a.out

buildtcc:
	cd tinycc && ./configure --enable-static
	cd tinycc && make libtcc.a
