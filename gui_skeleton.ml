open Pre
open Sttt_module_types
open Graphics

(* This is a function that invokes quite a few things from the Std Lib (which is
   why we wrote it for you), some of which is magic. No need to understand how
   the magic works. What init does is open the app window and returns the, in pixels
   size of the window. For various reasons, we won't be resizing the window. We're
   going to use it to sets the values of sx & sy for us. (Refer to the module type) *)

let init title = let open Graphics in
  open_graph "";
  (match Sys.os_type with "Unix" -> () | _ -> ignore (Unix.select [] [] [] 0.5));
  set_window_title title;
  size_x (), size_y ()

module GUI : StttUI = functor (M : StttModel) -> struct

  (* these types have basically just been copied over from the module type *)
  type button_type = 
	| Exit
	| SetPlayers
	| Test of int
	| Reset
	| Back
	| Forward
	| Testee
	| Interactive
	| AI1
	| AI2
	| AI3
  
  type click_result = 
	| Button of button_type
	| Board of M.mv
	| Nothing

  (* A stub is a function that has the correct signature, but not necessarily
     its correct implementation (for a given spec). Please redefine the
     following values as per the module type by replacing the stub definitions
     with real implementations. *)

  (* this slightly strange defn (even for a stub) is just to remind you that
     you can have any type you'd like as your draw_result *)
  let sx,sy = init "[YSC1212]  Super Tic-Tac-Toe"

  type draw_result = unit

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

  let draw g = 
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
    draw_rect ((170 * sx)/200) ((667 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((174 * sx)/200) ((707 * sy)/1000);
    draw_string "Test 100";
    draw_rect ((170 * sx)/200) ((547 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((173 * sx)/200) ((587 * sy)/1000);
    draw_string "AI Testee";
    draw_rect ((170 * sx)/200) ((427 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((177 * sx)/200) ((467 * sy)/1000);
    draw_string "Randy";
    draw_rect ((170 * sx)/200) ((307 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((177 * sx)/200) ((347 * sy)/1000);
    draw_string "Sandy";
    draw_rect ((170 * sx)/200) ((187 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((177 * sx)/200) ((227 * sy)/1000);
    draw_string "Reset";
    draw_rect ((170 * sx)/200) ((67 * sy)/1000) ((120 * sx)/1000) (sy/10);
    fill_poly [|(((189 * sx)/200),((117 * sy)/1000)) ; (((178 * sx)/200),((147 * sy)/1000)); (((178 * sx)/200),((87 * sy)/1000))|];
    draw_rect ((142 * sx)/200) ((667 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((147 * sx)/200) ((707 * sy)/1000);
    draw_string "Test 10";
    draw_rect ((142 * sx)/200) ((547 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((143 * sx)/200) ((587 * sy)/1000);
    draw_string "Set Players";
    draw_rect ((142 * sx)/200) ((427 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((151 * sx)/200) ((467 * sy)/1000);
    draw_string "Int";
    draw_rect ((142 * sx)/200) ((307 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((149 * sx)/200) ((347 * sy)/1000);
    draw_string "Tandy";
    draw_rect ((142 * sx)/200) ((187 * sy)/1000) ((120 * sx)/1000) (sy/10);
    moveto ((150 * sx)/200) ((227 * sy)/1000);
    draw_string "Exit";
    draw_rect ((142 * sx)/200) ((67 * sy)/1000) ((120 * sx)/1000) (sy/10);
    fill_poly [|(((147 * sx)/200),((117 * sy)/1000)) ; (((158 * sx)/200),((147 * sy)/1000)); (((158 * sx)/200),((87 * sy)/1000))|];;

  let wait_for_click_result n = 
let event = wait_next_event [Button_down] in 
if event.mouse_x >= ((142 * sx)/200) && event.mouse_x <= (((142 * sx)/200) + ((120 * sx)/1000)) && event.mouse_y >= ((187 * sy)/1000) &&  event.mouse_y <= (((187 * sy)/1000) + (sy/10))
then 
(Button Exit)
else 
if event.mouse_x >= ((170 * sx)/200) && event.mouse_x <= (((170 * sx)/200)+ ((120 * sx)/1000)) && event.mouse_y >= ((187 * sy)/1000) &&  event.mouse_y <= (((187 * sy)/1000) + (sy/10))
then (Button Reset)
else 
if event.mouse_x >=  ((142 * sx)/200) && event.mouse_x <= ( ((142 * sx)/200)+ ((120 * sx)/1000)) && event.mouse_y >= ((187 * sy)/1000) &&  event.mouse_y <= (((547 * sy)/1000) + (sy/10))
then (Button SetPlayers) else
(Nothing);;
  

  let feedback str     = ()
  (* Status of the game. For example, Last move played: (2,1),(2,3)   X to play in minigame (2,1) *)
  let printer s1 s2 s3 = ()
  (* Player status, on top of buttons e.g. clicking set game produces player 1 vs. player 2 *)
  let printer2 str     = ()
  (* Click status of buttons, below buttons e.g. clicking set players produces 
  Set Players... and interactive when a game is being played with the player *) 

end

let gui_completed = true

module X = (val (modgui (module GUI:StttUI) gui_completed) : StttUI)
module Ans = Gsrun(X)
