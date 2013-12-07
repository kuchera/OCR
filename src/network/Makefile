OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
EXEC= network
SRC= neuron.ml layer.ml graph.ml network.ml
OBJ= $(SRC:.ml=.cmx)
USED= stdlib.cmx iostream.cmx sdltools.cmx
USEDO= ${USED:.cmx=.o} ${USED:.cmx=.cmi}
MLI= neuron.mli layer.mli graph.mli
MAINPATH= ..
CURPATH= ./network
 
compile: modules
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${USED} ${OBJ}

modules: ${SRC} getobj
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -c ${MLI} ${SRC}

exe: compile rmobj

getobj:
	cd ${MAINPATH} && cp ${USED} ${USEDO} ${CURPATH}

main:
	cd ${MAINPATH} && make

all:
	cd ${MAINPATH} && make all

rmobj:
	rm -f *~ *.o *.cm?

clean: rmobj
	rm -f ${EXEC}

mrproper:
	cd ${MAINPATH} && make mrproper
	cd learning && make clean
