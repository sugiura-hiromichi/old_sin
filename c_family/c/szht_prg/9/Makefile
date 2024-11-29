file=$${NEOVIM_CXX_AUTO_RUNNED_FILE}

run: a.out
	./a.out

./a.out: $(NEOVIM_CXX_AUTO_RUNNED_FILE)
	rm -f a.out
	clang $(NEOVIM_CXX_AUTO_RUNNED_FILE)

clean:
	rm -f a.out
