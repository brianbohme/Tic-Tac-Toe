open Graphics 

open Unix

let init title = let open Graphics in
  open_graph "";
  (match Sys.os_type with "Unix" -> () | _ -> ignore (Unix.select [] [] [] 0.5));
  set_window_title title;
  size_x (), size_y ();;

let sx,sy = init "[YSC1212]  Super Tic-Tac-Toe";;

 let (mx, my) = (((3 * sx / 4) - 50), (sy - 20));;

  let (ux, uy) = ((mx / 3 - 5), (my / 3 - 5));;

  set_color (rgb 255 239 213);;

  fill_rect 0 0 sx sy;;

  set_color (rgb 025 025 112);;

  let draw_line x1 y1 x2 y2 = 
    moveto x1 y1; 
    lineto x2 y2;;

  let draw_rectangle x1 y1 x2 y2 =   
    draw_rect x1 y1 (x2-x1) (y2-y1);;

  let draw_button_wh x1 y1 w h =   
    draw_button x1 y1 (x1 + w) (y1 + h);;

  let draw_helper width height startx starty =
    draw_line (startx + width / 3) (starty) (startx + width / 3) (starty + height);
    draw_line (startx + 2 * width / 3) (starty) (startx + 2 * width / 3) (starty + height);
    draw_line (startx) (starty + height / 3) (startx + width) (starty + height / 3);
    draw_line (startx) (starty + 2 * height / 3) (startx + width) (starty + 2 * height / 3);;

  let draw_big_board width height startx starty = 
    set_line_width 6;
    draw_helper (width - 10) (height - 15) (startx + 5) (starty + 5);;

  let draw_small_board width height startx starty = 
    set_line_width 3;
    draw_helper (width - 30) (height - 30) (startx + 25) (starty + 25);;

    draw_big_board mx my 10 20;;
    draw_small_board ux uy 10 15;;
    draw_small_board ux uy 10 (my / 3 + 10);;
    draw_small_board ux uy 10 (2 * (my / 3) + 10);;
    draw_small_board ux uy (mx / 3) 15;;
    draw_small_board ux uy (mx / 3) (my / 3 + 10);;
    draw_small_board ux uy (mx / 3) (2 * (my / 3) + 10);;
    draw_small_board ux uy (2 * (mx / 3)) 15;;
    draw_small_board ux uy (2 * (mx / 3)) (my / 3 + 10);;
    draw_small_board ux uy (2 * (mx / 3)) (2 * (my / 3) + 10);;

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

wait_next_event[Button_down];;



    (* 
    ocamlfind ocamlc -package graphics,unix tester.ml -linkpkg -o tester 
  *)
