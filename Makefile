include Makefile.base

.PHONY: exec
exec: build
	stack exec -- midriff-exe
