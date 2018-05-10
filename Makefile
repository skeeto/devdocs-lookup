.POSIX:
EMACS = emacs

compile: devdocs-lookup.elc

clean:
	rm -f devdocs-lookup.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
