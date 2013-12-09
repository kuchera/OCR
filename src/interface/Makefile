OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl -I +site-lib/lablgtk2
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa lablgtk.cmxa
EXEC= interface 
SRC= interface.ml
OBJ= $(SRC:.ml=.cmx)
USED= stdlib.cmx iostream.cmx sdltools.cmx
USEDO= ${USED:.cmx=.o} ${USED:.cmx=.cmi}
MLI= 
MAINPATH= ..
CURPATH= ./interface
 
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
	rm -f ./files/network/detected/*.mat

mrproper:
	cd ${MAINPATH} && make mrproper

getl:
	cp ../detection/files/network/detected/* ./files/network/detected/
