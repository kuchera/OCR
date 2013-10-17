OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
EXEC= a.out
SRC= interface.ml
OBJ= $(SRC:.ml=.cmx)
USED= sdltools.cmx
MLI= 
 
compile: modules
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${USED} ${OBJ}

modules: ${SRC} getobj
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -c ${MLI} ${SRC}

exe: compile rmobj

getobj:
	cd .. && make
	cp ../sdltools.cmx ../sdltools.o ../sdltools.cmi ./
	cd .. && make clean

rmobj:
	rm -f *~ *.o *.cm?

clean: rmobj
	rm -f ${EXEC}
