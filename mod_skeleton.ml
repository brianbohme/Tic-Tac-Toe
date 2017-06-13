open Pre
open Sttt_module_types

module MOD : StttModel = struct 
  include StttMark 

  type lm_result =
    | Move of mv
    | Resign
    | Empty;;

   type global = {
    board     : mark list;
    to_move   : xo;
    last_move : lm_result;
    cell_tp   : sub_mv option;
    over      : sub_mark option
  };;

  type game_result =
    | Exceptional of (string * global)
    | Ok of global;;
 
 (* Inserts a move into a cell *)
  let insert_cell ttt cell_index mv = 
    match ttt with 
    | Open l -> Open (List.mapi (fun i y -> if i = cell_index then mv else y) l)
    | _ -> raise Not_found;;

(* Inserts an updated miniboard in the board *)
  let update_board sttt cell_index board_index mv = 
    List.mapi (fun i y -> if i = board_index then insert_cell y cell_index mv else y) sttt;;

(* Changes the coordinate into an integer position *)
  let indexer coordinate = 
    match coordinate with
      | (0, 0) -> 0
      | (0, 1) -> 1
      | (0, 2) -> 2
      | (1, 0) -> 3
      | (1, 1) -> 4
      | (1 ,2) -> 5
      | (2, 0) -> 6
      | (2, 1) -> 7
      | (2, 2) -> 8
      | (_, _) -> raise Not_found;;

(* Tests whether a winning condition has been met *)
  let win_test l player = 
    match l with 
    | [x; _; _; _; y; _; _; _; z] when x = y && y = z && x = Done player -> true
    | [_; _; x; _; y; _; z; _; _] when x = y && y = z && x = Done player -> true
    | [x; y; z; _; _; _; _; _; _] when x = y && y = z && x = Done player -> true
    | [_; _; _ ; x; y; z;_; _; _] when x = y && y = z && x = Done player -> true
    | [_; _; _; _; _; _; x; y; z] when x = y && y = z && x = Done player -> true
    | [x; _; _; y; _; _; z; _; _] when x = y && y = z && x = Done player -> true
    | [_; x; _; _; y; _; _; z; _] when x = y && y = z && x = Done player -> true
    | [_; _; x; _; _; y; _; _; z] when x = y && y = z && x = Done player -> true
    | _ -> false;;

(* Changes a board from Open to Done if won *)
  let win_updater l player = 
    match l with 
    | Open [x; _; _; _; y; _; _; _; z] when x = y && y = z && x <> B -> Done player
    | Open [_; _; x; _; y; _; z; _; _] when x = y && y = z && x <> B -> Done player
    | Open [x; y; z; _; _; _; _; _; _] when x = y && y = z && x <> B -> Done player
    | Open [_; _; _ ; x; y; z;_; _; _] when x = y && y = z && x <> B -> Done player
    | Open [_; _; _; _; _; _; x; y; z] when x = y && y = z && x <> B -> Done player
    | Open [x; _; _; y; _; _; z; _; _] when x = y && y = z && x <> B -> Done player
    | Open [_; x; _; _; y; _; _; z; _] when x = y && y = z && x <> B -> Done player
    | Open [_; _; x; _; _; y; _; _; z] when x = y && y = z && x <> B -> Done player
    | Open [P _; P _; P _; P _; P _; P _; P _; P _; P _] -> Done B
    | _ -> l;;

(* Inserts a win into the board *)
  let insert_win l player index = 
    List.mapi (fun i y -> if i = index then (win_updater y player) else y) l;;

(* Main insertion function for the overall move updater below *)
  let inserter l mvo player = 
    match mvo with 
      | Some ((w, x), (y, z)) -> 
          insert_win (update_board l (indexer (w, x)) (indexer (y, z)) player) player (indexer (y, z))
      | None -> raise Not_found;;

(* Tests whether a move is trying to be played in an invalid board *)
  let invalid_board l mv = 
    match List.nth l (indexer mv) with
    | Done _ -> false
    | Open _ -> true;;

(* Tests whether a move is trying to be played on an occupied space *)
  let just_board l = 
  match l with 
  | Open x -> x
  | Done y -> [y];;

  let index_list l = 
    List.mapi (fun i y -> (i, y)) l;;

  let rec isolate l mv = 
  let index = indexer mv in 
    match index_list l with
        | [] -> []
        | ((k, v)::t) when k = index -> v
        | ((k, v)::t) -> (isolate t mv);;

  let option_to_int x = 
  match x with
  | Some x -> x
  | None -> raise Not_found;;

  let invalid_spot l mvb mvc =
  try
    let sub_list = (isolate (index_list (List.map just_board l)) (option_to_int mvb)) in
    if sub_list = [P X] || sub_list = [P O] then false else
    match List.nth sub_list (indexer mvc) with 
    | P X -> false 
    | P O -> false 
    | _ -> true
with Not_found -> true;;

(* Tracks and updates if there are ties on the board *)
  let tie_tracker l = 
    match l with
    | [Done _; Done _; Done _; Done _; Done _; Done _; Done _; Done _; Done _ ] -> true
    | _ -> false;;

(* Main move inserter, updating the global *)
let execute_move g mvo =
  match g, mvo with {board = l; to_move = player; last_move = previous; 
    cell_tp = next; over = status}, Some ((w, x), (y, z)) -> 
    let x_move = inserter l mvo (P X) in 
    let o_move = inserter l mvo (P O) in 
    begin 
    match player with
    | X -> if next = None && invalid_spot l (Some (y,z)) (w,x) = false then 
    				Exceptional ("Invalid Move", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P O)}))
    		else
    		if invalid_spot l next (w,x) = true then
                Ok {board = x_move;
                to_move = O; 
                last_move = Move ((w, x), (y, z));
                cell_tp = 
                  if invalid_board x_move (w, x) = false then None 
                  else Some (w, x);
                over = 
                  if win_test x_move (P X) = true then Some (P X) 
                  else if tie_tracker x_move = true then Some B 
                  else None}
            else Exceptional ("Invalid Move", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P O)}))
    | O -> if next = None && invalid_spot l (Some (y,z)) (w,x) = false then 
    				Exceptional ("Invalid Move", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P X)}))
    		else
    		if invalid_spot l next (w,x) = true then
                Ok {board = o_move;
                to_move = X; 
                last_move = Move ((w, x), (y, z));
                cell_tp = 
                  if invalid_board o_move (w, x) = false then None 
                  else Some (w, x);
                over = 
                  if win_test o_move (P O) = true then Some (P O) 
                  else if tie_tracker o_move = true then Some B 
                  else None} 
               else Exceptional ("Invalid Move", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P X)}))
    end
  | _, _ -> raise Not_found;;

(* Main update function, takes a global and a move and tests for any invalid moves *)
  let update g mvo = 
  match g with {board = l; to_move = player; last_move = previous; 
    cell_tp = next; over = status} -> 
    begin 
    match player with 
    | X  -> begin try begin if mvo = None then Exceptional ("Player Resignation", 
                                    {board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P O)})
            else 
            begin 
            match next with
              | Some (v, u) -> begin 
                               match mvo with
                                | Some ((w, x), (y, z)) -> 
                                    if (y, z) = (v, u) && invalid_board l (y, z) = true  
                                    then execute_move g mvo
                                    else Exceptional ("Invalid Move", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P O)}))
                                | None -> raise Not_found 
                               end
              | None -> execute_move g mvo end end 
          with Not_found -> Exceptional ("Invalid Move", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P O)})) end
    | O -> begin try begin if mvo = None then Exceptional ("Player Resignation", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P X)}))
            else 
            begin 
            match next with 
              | Some (v, u) -> begin 
                               match mvo with
                                | Some ((w, x), (y, z)) -> 
                                    if (y, z) = (v, u) && invalid_board l (y, z) = true   
                                    then execute_move g mvo
                                    else Exceptional ("Invalid Move", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P O)}))
                                | None -> raise Not_found 
                               end
              | None -> execute_move g mvo 
            end end 
        with Not_found -> Exceptional ("Invalid Move", 
                                    ({board = l;
                                     last_move = Resign;
                                     cell_tp = None; 
                                     to_move = X;
                                     over = Some (P X)})) end
    end;;

end 

let mod_completed = true

module X = (val (modmod (module MOD:StttModel) mod_completed) : StttModel)
module Ans = Msrun(X)
