OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
EXEC= a.out
SRC= bruit.ml binarisation.ml filtre.ml
OBJ= $(SRC:.ml=.cmx)
MLI= 
 
compile: modules
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${OBJ}

modules: ${SRC}
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -c ${MLI} ${SRC}

exe: compile rmobj

rmobj:
	rm -f *~ *.o *.cm?

clean: rmobj
	rm -f ${EXEC}
