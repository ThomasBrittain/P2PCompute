# Get rid of the annyong tabs required by default
.RECIPEPREFIX = >

# Compile p2p_client to native code
all: compile_and_link clear_intermediate_files

# Compile and link
compile_and_link:
> ocamlfind ocamlopt -thread -syntax camlp4o \
> -package cohttp.lwt,uri,yojson,lwt,lwt.syntax,lwt.unix \
> -linkpkg -o ClientNode p2p_client.ml

clear_intermediate_files:
> rm *.cmi *.cmx *.o
