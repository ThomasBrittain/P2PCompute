# Get rid of the annyong tabs required by default
.RECIPEPREFIX = >

# Compile p2p_client to native code
all: compile_and_link clear_intermediate_files

byte: compile_and_link_byte clear_intermediate_files_byte

# Compile and link
compile_and_link:
> ocamlfind ocamlopt -thread -syntax camlp4o \
> -package cohttp.lwt,uri,yojson,lwt,lwt.syntax,lwt.unix,str,lablgtk2,lwt.glib \
> -linkpkg -o ClientNodeGui user_info.ml gui.ml p2p_client.ml gui_control.ml main.ml

# Make a bytecode executable
compile_and_link_byte:
> ocamlfind ocamlc -custom -thread -syntax camlp4o \
> -package cohttp.lwt,uri,yojson,lwt,lwt.syntax,lwt.unix,str,lablgtk2,lwt.glib \
> -linkpkg -o ClientNodeGui.byte user_info.ml gui.ml p2p_client.ml gui_control.ml main.ml

clear_intermediate_files:
> rm *.cmi *.cmx *.o

clear_intermediate_files_byte:
> rm *.cmi *.cmo
