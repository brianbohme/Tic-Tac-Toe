#!/bin/bash

ocamlfind ocamlc -package graphics,unix pre.cmo gui_skeleton.ml -linkpkg -o superttt_gui

./superttt_gui

rm gui_skeleton.cmi
rm gui_skeleton.cmo

rm superttt_gui
