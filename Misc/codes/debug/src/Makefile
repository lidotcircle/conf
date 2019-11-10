all: hwwp.o

hwwp.o: hwwp.c
	@gcc -std=gnu11 -c -g -O0 -o $@ $^

clean:
	@rm -f core hwwp.o
