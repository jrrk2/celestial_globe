atlas_generator_makefile: messier_data.ml texture_clustering.ml texture_atlas.ml atlas_generator_makefile.ml
	ocamlopt -I +unix unix.cmxa messier_data.ml texture_clustering.ml texture_atlas.ml atlas_generator_makefile.ml -o $@
