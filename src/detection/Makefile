OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
EXEC= detection
SRC= detect.ml
OBJ= $(SRC:.ml=.cmx)
USED= stdlib.cmx sdltools.cmx iostream.cmx
USEDO= ${USED:.cmx=.o} ${USED:.cmx=.cmi}
MLI= 
 
compile: modules
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${USED} ${OBJ}

modules: ${SRC} getobj
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -c ${MLI} ${SRC}

exe: compile rmobj

getobj:
	cd .. && cp ${USED} ${USEDO} ./detection/

rmobj:
	rm -f *~ *.o *.cm?

clean: rmobj
	rm -f ${EXEC}
