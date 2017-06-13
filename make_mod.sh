#!/bin/bash

ocamlfind ocamlc -package graphics,unix pre.cmo mod_skeleton.ml -linkpkg -o superttt_mod

./superttt_mod

rm mod_skeleton.cmi
rm mod_skeleton.cmo

rm superttt_mod
