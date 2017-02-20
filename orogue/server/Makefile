DIRECTORIES:=$(addprefix -I , $(shell find sources -type d))
SOURCES:=$(shell find sources -type f -name "*.ml")
INTF :=$(patsubst %.ml, %.inferred.mli, $(SOURCES))

all:
	ocamlbuild $(DIRECTORIES) -lib unix server.native
	ocamlbuild $(DIRECTORIES) -cflags -g -lib unix server.p.native
	ocamlbuild $(DIRECTORIES) -lib unix mapgen.native
	ocamlbuild $(DIRECTORIES) -cflags -g -lib unix mapgen.d.byte

genmap: all
	./mapgen.d.byte > world.dat

test: all
	OCAMLRUNPARAM=b ./mapgen.d.byte

intf:
	ocamlbuild $(DIRECTORIES) $(INTF)

clean:
	ocamlbuild -classic-display -clean
	rm -rf gmon.out
	#rm -rf gmon.out world.dat
