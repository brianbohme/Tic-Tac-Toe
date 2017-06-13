module type StttMark = sig
  type xo       = X | O
  type sub_mark = B | P of xo
  type sub_mv   = int*int
  type mark     = Done of sub_mark | Open of sub_mark list 
  type mv       = sub_mv * sub_mv
end

module StttMark = struct
  type xo       = X | O
  type sub_mark = B | P of xo
  type sub_mv   = int*int
  type mark     = Done of sub_mark | Open of sub_mark list 
  type mv       = sub_mv * sub_mv
end

module type StttMarkWithSide = sig include StttMark val side : xo end

module type StttModel = sig
  include StttMark 

  type lm_result = (* last move result *)
    | Move of mv
    | Resign
    | Empty

   type global = {
    board     : mark list;
    to_move   : xo;
    last_move : lm_result;
    cell_tp   : sub_mv option;
    over      : sub_mark option
  }

  type game_result =
    | Exceptional of (string * global)
    | Ok of global

  val update : global -> mv option -> game_result
end

module type StttPlayer = functor (M : StttMarkWithSide) -> sig
  val move : M.mv option -> M.mark list -> M.mv option
end

module type StttUI = functor (M : StttModel) -> sig

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

  type draw_result (* this can any type you'd like. e.g. unit / int*int*int*int / bool *)

  val sx : int
  val sy : int

  val wait_for_click_result : draw_result -> click_result
  val feedback  : string -> unit
  val printer   : string -> string -> string -> unit
  val printer2  : string -> unit
  val draw      : M.global -> draw_result

end
