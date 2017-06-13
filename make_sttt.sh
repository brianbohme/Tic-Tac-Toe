#!/bin/bash

ocamlfind ocamlc -package graphics,unix sttt.cmo -linkpkg -o superttt

./superttt

rm superttt
