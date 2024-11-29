clngp_opt= -std=c++2a -Wall -mmacosx-version-min=13 --pedantic-errors

r : a.out
	./a.out

t : b.out
	./b.out

a.out : main.cpp all.h all.h.pch
	clang++ $(clngp_opt) -include all.h $< -o $@


b.out : test.cpp all.h all.h.pch
	clang++ $(clngp_opt) -include all.h $< -o $@

all.h.pch : all.h
	clang++ $(clngp_opt) -x c++-header -o $@ $<

clean :
	rm -f ./a.out
	rm -f ./all.h.pch

.PHONY : r t clean