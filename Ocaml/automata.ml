(* 
  Function to replace last ' ' in the string for a '\n'.
  
  Input:
    . s -> string
  How it works:
    . If the string is not empty (length > 0) and the last character is equal to ' ', 
      returns a substring of s, starting at 0 with length n-1 concatenated with "\n".
    . Else, returns the initial string.
  Output:
    . Initial string or a substring of the initial one with "\n" in the last character.
*)
let replace_last_space (s:string) =
  let n = String.length s in
  if n > 0 && s.[n-1] = ' ' then (String.sub s 0 (n-1) ^ "\n") else s;;

(* 
  Function to transform a char in string.

  Input:
    . c -> char
  Output:
    . string of length 1 holding the character c
*)
let char_to_string (c:char) = String.make 1 c;;

(* 
  Function to get the index of an element in list.
  
  Input:
    . e -> Element of a list
    . l -> list
  Output:
    . Index of the element e in the list l
    
  Source: https://stackoverflow.com/questions/31279920/finding-an-item-in-a-list-and-returning-its-index-ocaml
*)
let index_of e l =
  let rec index_rec i = function
    | [] -> -1
    | hd::tl -> if hd = e then i else index_rec (i+1) tl
  in
  index_rec 0 l;;

(* 
  Function to read a line an put it inside a string array.
 
  Output:
    . string array containing values of the line
    
  Source: https://stackoverflow.com/questions/55716256/noob-question-on-reading-integer-string-inputs-in-ocaml
*)
let read_to_string_array () = 
  let line = read_line () in
  let strs = Str.split (Str.regexp " *") line in 
  Array.of_list(strs);;

(* 
  Function to read a line an put it inside a int array.
  
  Output:
    . int array containing values of the line
    
  Source: https://stackoverflow.com/questions/55716256/noob-question-on-reading-integer-string-inputs-in-ocaml
*)
let read_to_int_array () = 
  let line = read_line () in
  let ints = Str.split (Str.regexp " *") line in 
  Array.of_list(List.map int_of_string ints);;
 
(* 
  Function to read n transitions to a matrix that represents the automaton.
  
  Input:
    . num_transitions -> Number of transactions
    . n -> The size of the matrix
  How it works:
    . Creates a matrix (list array array) with n size, a temporary array ([|transition start state; character; arrival state|]), 
    reads a transaction, store it on the array, and concat the character to the list in the matrix.(l).(c)
  Output:
    . Matrix (automaton) containing the respective characters of the transacitons
*)  
let read_transitions (num_transitions:int) (n:int) =
  let mtx_3D = Array.init n (fun i -> Array.make n []) in
  for i = 0 to num_transitions - 1 do
    let transition = read_to_string_array () in
    let l = int_of_string(transition.(0)) - 1 in
    let c = int_of_string(transition.(2)) - 1 in
    mtx_3D.(l).(c) <- mtx_3D.(l).(c) @ [(transition.(1))] ;
  done;
  mtx_3D;;

(*    
  Function to check if there is or not duplicate chars in a line -> That state has multiple 
  connections by the shame char.

  Input:
    . l -> Line of the matrix (state)
    . mtx_3D -> The 3-dimensional matrix that represents the automaton
    . mtx_size -> The size of mtx_3D
  How it works:
    . Checks if there are 2 repeated chars in that line
  Output:
    . Returns true if exists two transictions with the same character in the line (state) l and false if not.
*)    
let duplicate_in_line (l:int) (mtx_3D: string list array array) (mtx_size: int) =
  let line_content = ref [] in
  try
    for c = 0 to mtx_size - 1 do 
      let cell = mtx_3D.(l).(c) in
      let cell_size = List.length cell in
      for pos = 0 to cell_size - 1 do
        let cell_char = List.nth cell pos in
        let char_in_line = List.mem cell_char !line_content in
        if char_in_line then raise Exit 
        else line_content := cell_char :: !line_content
      done;
    done;
    false;
  with Exit -> true;;

(*    
  Function to check if the matrix (automaton) has states that have multiple connections to others by the same char.
  
  Input:
    . mtx_3D -> The 3-dimensional matrix that represents the automaton
    . mtx_size -> The size of mtx_3D
  How it works:
    . Check each line, and see if exists in a line two transictions with same character in the mtx_3d
  Output:
    . Returns true if the matrix (automaton) has states that have multiple connections to others by the same char, false if not. 
*)    
let ndfa_condition1 (mtx_3D: string list array array) (mtx_size: int) = 
  try
    for l = 0 to mtx_size - 1 do
      if duplicate_in_line l mtx_3D mtx_size then raise Exit 
    done;
    false;
  with Exit -> true;;

(*    
  Function to check if the matrix (automaton) has empty transitions.

  Input:
    . mtx_3D -> The 3-dimensional matrix that represents the automaton
    . mtx_size -> The size of mtx_3D
  How it works:
    . Check each cell in the matrix, if finds an "_" in a cell returns true, else return false
  Output:
    . Returns true if the matrix (automaton) has empty connections
*)    
let ndfa_condition2 (mtx_3D: string list array array) (mtx_size: int) = 
  try
    for l = 0 to mtx_size - 1 do
      for c = 0 to mtx_size - 1 do
        let cell = mtx_3D.(l).(c) in
        let exist_empty_transaction = List.mem "_" cell in
        if exist_empty_transaction then raise Exit
      done;
    done;
    false;
  with Exit -> true;;

(*    
  Function to check if the number of elements of the set of initial states is more than 1.

  Input:
    . card_So -> Cardinality of the set of initial states
  Output:
    . True if exists more than one initial state and False otherwise
*)  
let ndfa_condition3 (card_S0: int) : bool = 
  if card_S0 > 1 then true else false;;

(*    
  Function to check if the automaton is NDFA or DFA.

  Input:
    . condition1 -> Representing the condition 1 (More than one transaction to other states by the same character)
    . condition2 -> Representing the condition 2 (Exists empty transactions)
    . condition3 -> Representing the condition 3 (More than one initial state)
    (the order can change)
  How it works:
    . If, at least, one condition is true than it is a non-deterministic finite automaton else it is a deterministic finite automaton
  Output:
    . NDFA -> Non-deterministic finite automaton
    or
    . DFA  -> Deterministic finite automaton
*)      
let ndfa_or_dfa (condition1 : bool) (condition2 : bool) (condition3 : bool) =
  if condition1 || condition2 || condition3 then "NDFA" else "DFA"

(*  
  Function to check if a certain goal exists in a line.

  Input:
    . mtx_3D -> The 3-dimensional matrix that represents the automaton
    . mtx_size -> The size of mtx_3D
    . l -> Line l
    . goal -> Character to find
  How it works:
    . Checks if character goal exists in any column of the line l
  Output:
    . True if finds character goal in a column of the line l, False if not
*)     
let goal_exists_in_line (mtx_3D: string list array array) (mtx_size: int) (l: int) (goal: string) =
  try
    for c = 0 to mtx_size - 1 do
      let cell = mtx_3D.(l - 1).(c) in
      if (index_of goal cell <> - 1) then raise Exit;
    done;
    false;
  with Exit-> true;;

(*    
  Function to get the columns that contain the goal, in a certain line.

  Input:
    . mtx_3D -> The 3-dimensional matrix that represents the automaton
    . mtx_size -> The size of mtx_3D
    . l -> Line l
    . goal -> character to find
  How it works:
    . Add the column number that has the goal to a list.
  Output:
    . List that contains all columns that contain the character goal
*)    
let get_columns_of_goal (mtx_3D: string list array array) (mtx_size: int) (l: int) (goal: string) =
  let columns_with_goal = ref [] in
  for c = 0 to mtx_size - 1 do
    let cell = mtx_3D.(l - 1).(c) in
    if (index_of goal cell <> -1) then (
      columns_with_goal := (c + 1) :: !columns_with_goal
    )   
  done;
  !columns_with_goal;;

(* 
  Function to search for all the possible paths that recognize the word.

  Input:
    . mtx_3D -> The 3-dimensional matrix that represents the automaton
    . mtx_size -> The size of mtx_3D
    . line -> The line of the matrix where we will search for the goal
    . goal -> The char (of the word) that we are looking for
    . word_pos -> The position of our goal, in the word
    . current_word -> The word that we are building
    . word -> The word that we want to recognize
    . path -> The path to the word
    . all_paths -> All the possible paths to the word
  How it works:
    . If current_word = word then:
      . If the current state have some ϵ transitions, follow them
      . else, add the path to all_paths
    . Else:
      . If there is a transition(s) that carrie(s) my goal, follow them
      . Else 
        . If there is a transition(s) that carrie(s) ϵ, follow them 
        . Else, do nothing
  Output:
    . Nothing
*)
let rec find_path_rec (mtx_3D: string list array array) (mtx_size: int) (line:int) (goal:string) (word_pos: int) (current_word: string) (word: string) (path: string) (all_paths: string list ref) = 
  if current_word = word then (
    let columns_goal = get_columns_of_goal mtx_3D mtx_size line "_" in
    if (List.length columns_goal <> 0) then (
      for count = 0 to (List.length columns_goal)-1 do
        let line_of_search = List.nth columns_goal count in
        let updated_path = (path ^ (string_of_int (List.nth columns_goal count)) ^ " ") in
        find_path_rec mtx_3D mtx_size line_of_search goal word_pos current_word word updated_path all_paths;
      done;
    ) else (
      all_paths := (replace_last_space path) :: !all_paths;
    )
  ) else (
    if goal_exists_in_line mtx_3D mtx_size line goal then (
      let columns_goal = get_columns_of_goal mtx_3D mtx_size line goal in
      for count = 0 to (List.length columns_goal)-1 do
        let new_word_pos = (if word_pos + 1 = String.length word then word_pos else word_pos + 1) in
        let line_of_search = List.nth columns_goal count in
        let new_current_word = (current_word ^ (char_to_string (String.get word word_pos))) in
        let updated_path = (path ^ (string_of_int (List.nth columns_goal count)) ^ " ") in
        let new_goal = (char_to_string (String.get word new_word_pos)) in
        find_path_rec mtx_3D mtx_size line_of_search new_goal new_word_pos new_current_word word updated_path all_paths;
      done;
    ) else (
      let columns_goal = get_columns_of_goal mtx_3D mtx_size line "_" in
      if (List.length columns_goal <> 0) then (
        for count = 0 to (List.length columns_goal)-1 do
          let line_of_search = List.nth columns_goal count in
          let updated_path = (path ^ (string_of_int (List.nth columns_goal count)) ^ " ") in
          find_path_rec mtx_3D mtx_size line_of_search goal word_pos current_word word updated_path all_paths;
        done;
      )
    );
  );;

(*    
  Function to check if any of the paths are recognized by the automaton.

  Input:
    . all_possible_paths -> All the possible paths to the word
    . set_F -> The final set of the automaton
  How it works:
    . For each path, check if the last state of that path belongs to the
    set_F. If yes, then returns that path. If not, return "NO".
  Output:
    . The path regonized by the automaton or "NO".
*)  
let recognizes_word (all_possible_paths: string list) (set_F: int array) =
  let path = ref "" in
  try
    for pos = 0 to (List.length all_possible_paths) - 1 do
      path := List.nth all_possible_paths pos;
      let pos_last_node = (String.length !path) - 2 in
      let last_node_of_path = char_to_string (String.get !path pos_last_node) in
      if index_of (int_of_string last_node_of_path) (Array.to_list set_F) <> -1 then raise Exit;
    done;
  "NO"
  with Exit-> !path;;

(* MAIN*)

(* INPUTS *)
let card_S = read_int();;
let card_S0 = read_int();;
let set_S0 = read_to_int_array ();;
let card_F = read_int();;
let set_F = read_to_int_array ();;
let n_transitions = read_int();;
let mtx_3D = read_transitions n_transitions card_S;;
let word = read_line();;

(* HEAVY WORK *)
let cond1_ndfa = ndfa_condition1 mtx_3D card_S;;
let cond2_ndfa = ndfa_condition2 mtx_3D card_S;;
let cond3_ndfa = ndfa_condition3 card_S0;;
let avaliation_ndfa_dfa = ndfa_or_dfa cond1_ndfa cond2_ndfa cond3_ndfa;;

let all_possible_paths =  ref [];;
find_path_rec mtx_3D card_S 1 (char_to_string (String.get word 0)) 0 "" word "1 " all_possible_paths;;

let recognizes_word = recognizes_word !all_possible_paths set_F;;

(* OUTPUT *)
Printf.printf "%s\n" avaliation_ndfa_dfa;; (*LINE 1*)
if recognizes_word = "NO" then (
  Printf.printf "NO\n" (*LINE 2*)
) else (
  Printf.printf "YES\n"; (*LINE 2*)
  Printf.printf "%s" recognizes_word; (*LINE 3*)
);;

(* WORKING EXAMPLE *)

(*

INPUT:

6
3
1 2 6
3
3 4 6
10
1 _ 2
1 _ 3
2 a 4
2 b 1
3 _ 2
3 a 4
3 a 5
3 b 4
4 a 2
6 a 6
bbaa

card_S = 6
card_S0 = 3
set_S0 = [|1; 2; 6|]
card_F = 3
set_F = [|3; 4; 6|]
n_transitions = 10
mtx_3D = [|[[]; ['_']; ['_']; []; []; []],
           [['b']; []; []; ['a']; []; []],
           [[]; ['_']; []; ['a','b']; ['a']; []],
           [[]; ['a']; []; []; []; []],
           [[]; []; []; []; []; []],
           [[]; []; []; []; []; ['a']]|]
word = "bbaa"

-------------------------------------------------------------------

HEAVY WORK:

cond1_ndfa = true
cond2_ndfa = true
cond3_ndfa = true
avaliation_ndfa_dfa = "NDFA"

all_possible_paths = []

find_path_rec mtx_3D 6 1 "b" 0 "" "bbaa" "1 " []
  find_path_rec mtx_3D 6 2 "b" 0 "" "bbaa" "1 2" [];
    find_path_rec mtx_3D 6 1 "b" 1 "b" "bbaa" "1 2 1" [];
      find_path_rec mtx_3D 6 2 "b" 1 "b" "bbaa" "1 2 1 2" [];
        find_path_rec mtx_3D 6 1 "bb" 2 "a" "bbaa" "1 2 1 2 1" [];
          find_path_rec mtx_3D 6 2 "bb" 2 "a" "bbaa" "1 2 1 2 1 2" [];
            find_path_rec mtx_3D 6 4 "bba" 3 "a" "bbaa" "1 2 1 2 1 2 4" [];
              find_path_rec mtx_3D 6 2 "bbaa" 4 "a" "bbaa" "1 2 1 2 1 2 4 2" [];
                all_possible_paths = ["1 2 1 2 1 2 4 2"]
          find_path_rec mtx_3D 6 3 "bb" 2 "a" "bbaa" "1 2 1 2 1 3" [];
            find_path_rec mtx_3D 6 4 "bba" 3 "a" "bbaa" "1 2 1 2 1 3 4" [];
              find_path_rec mtx_3D 6 2 "bbaa" 3 "a" "bbaa" "1 2 1 2 1 3 4 2" [];
                all_possible_paths = ["1 2 1 2 1 2 4 2", "1 2 1 2 1 3 4 2"]
            find_path_rec mtx_3D 6 5 "bba" 3 "a" "bbaa" "1 2 1 2 1 3 5" [];
              do nothing
      find_path_rec mtx_3D 6 3 "b" 1 "b" "bbaa" "1 2 1 3" [];
        find_path_rec mtx_3D 6 4 "bb" 2 "a" "bbaa" "1 2 1 3 4" [];
          find_path_rec mtx_3D 6 2 "bba" 3 "a" "bbaa" "1 2 1 3 4 2" [];
            find_path_rec mtx_3D 6 4 "bbaa" 3 "a" "bbaa" "1 2 1 3 4 2 4" [];
              all_possible_paths = ["1 2 1 2 1 2 4 2", "1 2 1 2 1 3 4 2", "1 2 1 3 4 2 4"]
  find_path_rec mtx_3D 6 3 "b" 0 "" "bbaa" "1 3" [];
    find_path_rec mtx_3D 6 4 "b" 1 "b" "bbaa" "1 3 4" [];
      do nothing

all_possible_paths = ["1 2 1 2 1 2 4 2", "1 2 1 2 1 3 4 2", "1 2 1 3 4 2 4"]

recognizes_word = "1 2 1 3 4 2 4"

-------------------------------------------------------------------

OUTPUT:

NDFA
YES
1 2 1 3 4 2 4

*)
