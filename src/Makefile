OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
EXEC= run
SRC= sdltools.ml stdlib.ml iostream.ml main.ml
OBJ= $(SRC:.ml=.cmx)
MLI= sdltools.mli stdlib.mli iostream.mli
 
compile: modules
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${OBJ}

modules: ${SRC}
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -c ${MLI} ${SRC}

exe:: compile rmobj

all:: compile
	cd ./network && make
	cd ./interface && make
	cd ./filtres && make
	cd ./rotation && make
	cd ./detection && make

allexe: all
	cd network && make rmobj
	cd interface && make rmobj
	cd filtres && make rmobj
	cd rotation && make rmobj
	cd detection && make rmobj
	make rmobj

out: allexe
	rm -rf out/
	mkdir out
	cp ${EXEC} out/
	cp interface/interface out/
	cp filtres/filtres out/
	cp rotation/rotation out/
	cp detection/detection out/
	cp network/network out/
	make rmexe
	cp -r files/ out/files/

rmobj:
	rm -f *~ *.o *.cm?

rmexe:
	rm -f ${EXEC} 
	rm -f interface/interface
	rm -f filtres/filtres
	rm -f rotation/rotation
	rm -f detection/detection
	rm -f network/network 

clean:: rmobj
	rm -f ${EXEC}
	rm -rf out/

mrproper: clean
	cd network && make clean
	cd interface && make clean
	cd filtres && make clean
	cd rotation && make clean
	cd detection && make clean
