all: echo.lazy even_lines.lazy hello.lazy tarai.lazy reverse_lines.lazy fizzbuzz.lazy

%.lazy : %.hs
	../hs2lazy Prelude.hs $< > $@
