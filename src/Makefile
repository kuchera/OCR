OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
EXEC= a.out
SRC= sdltools.ml stdlib.ml iostream.ml serialization.ml main.ml
OBJ= $(SRC:.ml=.cmx)
MLI= sdltools.mli stdlib.mli iostream.mli serialization.mli
 
compile: modules
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${OBJ}

modules: ${SRC}
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -c ${MLI} ${SRC}

exe: compile rmobj

network:
	cd ./network && make

interface: 	
	cd ./interface && make

filtres:
	cd ./filtres && make

rotation:
	cd ./rotation && make

all: compile network interface filtres rotation

rmobj:
	rm -f *~ *.o *.cm?

clean: rmobj
	rm -f ${EXEC}

mrproper: clean
	cd network && make clean
	cd interface && make clean
	cd filtres && make clean
	cd rotation && make clean
