r : a.out
	./a.out

t : b.out
	./b.out

a.out : main.c
	clang $< -o $@

b.out : test.c
	clang $< -o $@

clean :
	rm -f ./a.out

.PHONY : r t clean