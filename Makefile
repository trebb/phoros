LISP = ../sbcl/bin/sbcl
RM = /bin/rm

SOURCE = *.lisp *.asd

phoros: $(SOURCE)
	$(LISP) --load make.lisp

clean:
	$(RM) *.fasl *.log phoros