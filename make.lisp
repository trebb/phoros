;;;; Make an executable.
;; TODO: don't expect quicklisp to be preloaded

(push (make-pathname :directory '(:relative :up "photogrammetrie")) asdf:*central-registry*)
(push (make-pathname :directory '(:relative :up "phoros")) asdf:*central-registry*)
(ql:quickload "phoros")
(in-package :phoros)
(sb-ext:save-lisp-and-die "phoros" :save-runtime-options t :toplevel (function main) :executable t)
