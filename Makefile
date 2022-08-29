CC = clang
CFLAGS = -Wall -Wextra
CFLAGS += -ggdb -fsanitize=address
LINKFLAGS = 

all: emulator
	./emulator

emulator: main.c
	$(CC) $(CFLAGS) $(LINKFLAGS) $^ -o $@

as: as.c assemblar.c common.o
	$(CC) $(CFLAGS) $(LINKFLAGS) $^ -o $@

common.o: common.c
	$(CC) $(CFLAGS) -c $^ -o $@

clean:
	rm -rf as i8080 common.o
