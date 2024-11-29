run : a.out
	./a.out

a.out : main.c
	clang $<

clean :
	rm -f ./a.out

.PHONY : run clean