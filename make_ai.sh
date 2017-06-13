#!/bin/bash

ocamlfind ocamlc -package graphics,unix pre.cmo ai_skeleton.ml -linkpkg -o superttt_ai

./superttt_ai

rm ai_skeleton.cmi
rm ai_skeleton.cmo

rm superttt_ai

