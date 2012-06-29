SRC = Lexer.hs Parser.hs Syntax.hs PPrint.hs Static.hs SCC.hs \
	Type.hs PatComp.hs Compiler.hs Optimizer.hs Builtin.hs Main.hs

GHCFLAGS =
OPTFLAGS = -O
#PROFFLAGS = -prof -auto-all

all: hs2lazy examples

hs2lazy: $(SRC)
	ghc $(GHCFLAGS) $(OPTFLAGS) $(PROFFLAGS) -o $@ --make Main.hs

Lexer.hs: Lexer.x
	alex Lexer.x

clean:
	rm -f *.o *.hi hs2lazy hs2lazy.exe

examples: hs2lazy
	make -C examples