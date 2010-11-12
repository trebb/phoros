LISP = ../sbcl/bin/sbcl
SOURCE = *.lisp *.asd

phoros: $(SOURCE)
	$(LISP) --load make.lisp
