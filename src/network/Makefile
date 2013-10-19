OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
EXEC= network
SRC=  neuron.ml layer.ml network.ml
OBJ= $(SRC:.ml=.cmx)
USED= stdlib.cmx iostream.cmx serialization.cmx sdltools.cmx
USEDO= ${USED:.cmx=.o} ${USED:.cmx=.cmi}
MLI= neuron.mli layer.mli
 
compile: modules
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${USED} ${OBJ}

modules: ${SRC} getobj
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -c ${MLI} ${SRC}

exe: compile rmobj

getobj:
	cd .. && cp ${USED} ${USEDO} ./network/

rmobj:
	rm -f *~ *.o *.cm?

clean: rmobj
	rm -f ${EXEC}
