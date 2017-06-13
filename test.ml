open Graphics 

open Unix

let init title = let open Graphics in
  open_graph "";
  (match Sys.os_type with "Unix" -> () | _ -> ignore (Unix.select [] [] [] 0.5));
  set_window_title title;
  size_x (), size_y ();;

let sx,sy = init "[YSC1212]  Super Tic-Tac-Toe";;

(* background (rgb 255 239 213);; *)

set_line_width 3;;

set_text_size 2;; 

set_color (rgb 25 25 112);;

(* Test 100 *)
draw_rect ((170 * sx)/200) ((667 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((174 * sx)/200) ((707 * sy)/1000);;

draw_string "Test 100";;

(* AI Testee *)
draw_rect ((170 * sx)/200) ((547 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((173 * sx)/200) ((587 * sy)/1000);;

draw_string "AI Testee";;

(* Randy *)
draw_rect ((170 * sx)/200) ((427 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((177 * sx)/200) ((467 * sy)/1000);;

draw_string "Randy";;

(* Sandy *)
draw_rect ((170 * sx)/200) ((307 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((177 * sx)/200) ((347 * sy)/1000);;

draw_string "Sandy";;

(* Reset *)
draw_rect ((170 * sx)/200) ((187 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((177 * sx)/200) ((227 * sy)/1000);;

draw_string "Reset";;

(* Forward *)
draw_rect ((170 * sx)/200) ((67 * sy)/1000) ((120 * sx)/1000) (sy/10);;

fill_poly [|(((189 * sx)/200),((117 * sy)/1000)) ; (((178 * sx)/200),((147 * sy)/1000)); (((178 * sx)/200),((87 * sy)/1000))|];;

(* Test 10 *)
draw_rect ((142 * sx)/200) ((667 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((147 * sx)/200) ((707 * sy)/1000);;

draw_string "Test 10";;

(* Set Players *)
draw_rect ((142 * sx)/200) ((547 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((143 * sx)/200) ((587 * sy)/1000);;

draw_string "Set Players";;

(* Int *)
draw_rect ((142 * sx)/200) ((427 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((151 * sx)/200) ((467 * sy)/1000);;

draw_string "Int";;

(* Tandy *)
draw_rect ((142 * sx)/200) ((307 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((149 * sx)/200) ((347 * sy)/1000);;

draw_string "Tandy";;

(* Exit *)
draw_rect ((142 * sx)/200) ((187 * sy)/1000) ((120 * sx)/1000) (sy/10);;

moveto ((150 * sx)/200) ((227 * sy)/1000);;

draw_string "Exit";;

(* Back *)
draw_rect ((142 * sx)/200) ((67 * sy)/1000) ((120 * sx)/1000) (sy/10);;

fill_poly [|(((147 * sx)/200),((117 * sy)/1000)) ; (((158 * sx)/200),((147 * sy)/1000)); (((158 * sx)/200),((87 * sy)/1000))|];;
 
(* 1 *)

moveto ((30 * sx)/1000) ((815 * sy)/1000);;

lineto ((215 * sx)/1000) ((815 * sy)/1000);;

moveto ((30 * sx)/1000) ((745 * sy)/1000);;

lineto ((215 * sx)/1000) ((745 * sy)/1000);;

moveto ((92 * sx)/1000) ((885 * sy)/1000);;

lineto ((92 * sx)/1000) ((675 * sy)/1000);;

moveto ((154 * sx)/1000) ((885 * sy)/1000);;

lineto ((154 * sx)/1000) ((675 * sy)/1000);;

(* 2 *)

moveto ((260 * sx)/1000) ((815 * sy)/1000);;

lineto ((445 * sx)/1000) ((815 * sy)/1000);;

moveto ((260 * sx)/1000) ((745 * sy)/1000);;

lineto ((445 * sx)/1000) ((745 * sy)/1000);;

moveto ((322 * sx)/1000) ((885 * sy)/1000);;

lineto ((322 * sx)/1000) ((675 * sy)/1000);;

moveto ((384 * sx)/1000) ((885 * sy)/1000);;

lineto ((384 * sx)/1000) ((675 * sy)/1000);;

(* 3 *)

moveto ((490 * sx)/1000) ((815 * sy)/1000);;

lineto ((675 * sx)/1000) ((815 * sy)/1000);;

moveto ((490 * sx)/1000) ((745 * sy)/1000);;

lineto ((675 * sx)/1000) ((745 * sy)/1000);;

moveto ((552 * sx)/1000) ((885 * sy)/1000);;

lineto ((552 * sx)/1000) ((675 * sy)/1000);;

moveto ((614 * sx)/1000) ((885 * sy)/1000);;

lineto ((614 * sx)/1000) ((675 * sy)/1000);;

(* 4 *)

moveto ((30 * sx)/1000) ((547 * sy)/1000);;

lineto ((215 * sx)/1000) ((547 * sy)/1000);;

moveto ((30 * sx)/1000) ((477 * sy)/1000);;

lineto ((215 * sx)/1000) ((477 * sy)/1000);;

moveto ((92 * sx)/1000) ((617 * sy)/1000);;

lineto ((92 * sx)/1000) ((397 * sy)/1000);;

moveto ((154 * sx)/1000) ((617 * sy)/1000);;

lineto ((154 * sx)/1000) ((397 * sy)/1000);;

(* 5 *)






(* Board *)

set_line_width 4;;

moveto ((10 * sx)/1000) ((646 * sy)/1000);;

lineto ((700 * sx)/1000) ((646 * sy)/1000);;

moveto ((10 * sx)/1000) ((333 * sy)/1000);;

lineto ((700 * sx)/1000) ((333 * sy)/1000);;

moveto ((240 * sx)/1000) ((940 * sy)/1000);;

lineto ((240 * sx)/1000) ((40 * sy)/1000);;

moveto ((470 * sx)/1000) ((940 * sy)/1000);;

lineto ((470 * sx)/1000) ((40 * sy)/1000);;

wait_next_event[Button_down];;

(* To compile: 
ocamlfind ocamlc -package graphics,unix test.ml -linkpkg -o test 

To run:
./test *)
