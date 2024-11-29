clang_opt= -mmacosx-version-min=13

r : a.out
	./a.out

b : main.c
	clang $(clang_opt) $<

t : b.out
	./b.out

a.out : main.c
	clang $(clang_opt) $< -o $@

b.out : test.c
	clang $(clang_opt) $< -o $@

clean :
	rm -f ./a.out

.PHONY : r t clean
