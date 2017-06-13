open Pre
open Sttt_module_types

module AI : StttPlayer = functor (M : StttMarkWithSide) -> struct

(* Takes a coordinate and assigns an integer *)
  let indexer k = 
      match k with
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

(* Takes an integer and assigns a coordinate *)
  let deindexer k = 
    match k with 
    | 0 -> (0, 0)
    | 1 -> (0, 1)
    | 2 -> (0, 2)
    | 3 -> (1, 0)
    | 4 -> (1, 1)
    | 5 -> (1 ,2)
    | 6 -> (2, 0)
    | 7 -> (2, 1)
    | 8 -> (2, 2)
    | _ -> raise Not_found;;
  
(* Inserts a move into a cell *)
  let insert_cell ttt cell_index mv = 
    match ttt with 
    | Open l -> Open (List.mapi (fun i y -> if i = cell_index then mv else y) l)
    | _ -> raise Not_found;;

(* Inserts an updated miniboard in the board *)
  let update_board sttt cell_index board_index mv = 
    List.mapi (fun i y -> if i = board_index then insert_cell y cell_index mv else y) sttt;;
  
(* Calculates point total of a row *)
  let row_evaluator l = 
    match l with
    | [x; y; z] -> 
    	     if x = y && y = z && x <> B then 1000
             else if x <> B && x = y && z = B || y <> B && z = y && x = B || z <> B && z = x && y = B then 100
             else if x <> B && y = z && y = B || y <> B && x = z && x = B || z <> B && y = x && y = B then 10
             else if y = x && x = z && z = B then 1
             else 0
    | _ -> raise Not_found;;

(* Calculates point total of a board *)
  let board_evaluator board = 
    match board with
    | Open [a; b; c; d; e; f; g; h; i] -> 
      row_evaluator [a; e; i] + row_evaluator [c; e; g] + 
      row_evaluator [a; b; c] + row_evaluator [d; e; f] + 
      row_evaluator [g; h; i] + row_evaluator [a; d; g] + 
      row_evaluator [b; e; h] + row_evaluator [c; f; i]
    | Done _ -> 1000
    | _ -> raise Not_found;;

(* Find blanks on a miniboard *)
  let rec mini_blanks board acc =
    match board with 
    | Open l -> begin match l with 
          | h :: t -> if h = B then acc :: (mini_blanks (Open t) (acc + 1))
                else mini_blanks (Open t) (acc + 1)
          | [] -> [] end
    | _ -> raise Not_found;; 

(* Find blanks on a board *)
  let rec main_blanks board acc = 
    match board with 
    | h :: t -> begin match h with
          | Open _ -> (List.map (fun x -> (x, acc)) (mini_blanks h 0)) :: (main_blanks t (acc + 1))
          | Done _ -> main_blanks t (acc + 1) end
    | [] -> [];;

(* Functions that evaluate a board/move based on the total/row valuation *)
  let rec pick_best_move l = 
    match l with 
    | h :: h1 :: t -> 
    	begin 
    	match h, h1 with
        | (Open _, Some _, x), (Open _, Some _, y) -> 
        	if x > y then pick_best_move (h :: t)
            else pick_best_move (h1 :: t)
        | _ -> raise Not_found 
    	end
    | h :: t -> 
    	begin 
    	match h with 
        | (Open _, Some w, _) -> Some w 
        | _ -> raise Not_found 
    	end
    | [] -> raise Not_found;;

  let rec pick_best_move_main l =
    match l with
    | h :: h1 :: t -> 
    	begin 
    	match h, h1 with
        | (_, Some _, x), (_, Some _, y) -> 
        	if x > y then pick_best_move_main (h :: t)
            else pick_best_move_main (h1 :: t)
        | _ -> raise Not_found end
    | h :: t -> 
    	begin 
    	match h with 
        | (_, Some w, _) -> Some w 
        | _ -> raise Not_found 
    	end
    | [] -> raise Not_found;;

(* Series of functions that assign valuation scores to a move and/or board *)
  let rec assign_points l = 
    match l with 
    | h :: t -> 
    	begin 
    	match h with
        | (l', x) -> (l', x, board_evaluator l') :: assign_points t 
    	end
    | [] -> [];;

  let rec points_adder l = 
    match l with 
    | h :: t -> (board_evaluator h) + points_adder t
    | [] -> 0;;

  let rec points_total l =
    match l with
    | h :: t -> 
    	begin 
    	match h with
        | (l', game) -> (l', game, points_adder l') :: points_total t 
    	end
    | [] -> [];;

(* Series of functions that find the best move *)
  let rec best_move_main board player remove_index = 
    match remove_index with
    | h :: t -> 
    	begin 
    	match h with
        | h1 :: t1 -> 
        	begin 
        	match h1 with 
            | (mini, main) -> ((update_board board mini main player), (Some (deindexer mini, deindexer main))) :: best_move_main board player (t1 :: t) 
        	end
        | [] -> [] 
    	end
    | [] -> [];;

  let rec best_move_miniboard board player remove_index game = 
    match remove_index with 
    | h :: t -> ((insert_cell board h player), (Some (deindexer h, game))) :: best_move_miniboard board player t game
    | [] -> [];;

  let bm_main board player remove_index =
    pick_best_move_main (points_total (best_move_main board player remove_index));;

  let bm_miniboard board player remove_index game =
    pick_best_move (assign_points (best_move_miniboard board player remove_index game));;

  let find_best_move board last player =
      match last with
      | Some ((w, x), (y, z)) -> 
      let t = (List.nth board (indexer (w, x))) in 
            begin 
            match t with
           	| Done _ -> bm_main board player (main_blanks board 0)
            | Open _ -> bm_miniboard t player (mini_blanks t 0) (w, x) 
        	end
      | None -> raise Not_found;;

(* Main move function *)
  let move (last : M.mv option) (board : M.mark list) : M.mv option = 
  	if M.side = X then (* you want to maximize score i.e. offensive player *)
    	begin 
      	match last with
      	| None -> Some ((1, 1), (1, 1))
      	| Some ((w, x), (y, z)) -> find_best_move board (Some ((w, x), (y, z))) (P X) 
  	  	end
    else (* You are O, want to minimize score i.e. defensive player *)
    	begin 
    	match last with
      	| None -> Some ((1, 1), (1, 1))
      	| Some ((w, x), (y, z)) -> find_best_move board (Some ((w, x), (y, z))) (P O) 
      	end;;

end

let ai_completed = true

module X = (val (modai (module AI:StttPlayer) ai_completed) : StttPlayer)
module Ans = Asrun(X)