CC = clang
CFLAGS = -Wall -Wextra

all: emulator
	./emulator

emulator: src/main.c
	$(CC) $(CFLAGS) $^ -o $@