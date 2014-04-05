all: echo.lazy even_lines.lazy hello.lazy tarai.lazy reverse_lines.lazy fizzbuzz.lazy lisp.lazy

%.lazy : %.hs
	../hs2lazy hs2lazy-prelude.hs $< > $@
