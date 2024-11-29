r : a.out
	./a.out

t : b.out
	./b.out

ar : c.out
	rm -rf ./c.txt
	./c.out > c.txt

p : a.out
	rm -rf ./a.txt
	./a.out > a.txt

a.out : main.c
	clang $< -o $@

b.out : test.c
	clang $< -o $@

c.out : rep_2.c
	clang $< -o $@

clean :
	rm -f *.out
	rm -f *.txt

.PHONY : r t ar p clean
