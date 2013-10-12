OCAML=ocamlopt
OCAMLFLAGS=
OCAMLLD=
EXEC= a.out
SRC= stdlib.ml iostream.ml main.ml
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
