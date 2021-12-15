let text lst = 
  List.nth lst 0
let slovar lst = match lst with
  |x :: y :: xs -> xs
  |_ -> failwith "neki je narobe"
let split_string_in_list str = match (String.split_on_char ' ' (String.trim str)) with
  |[a;_;b] -> [a;b]
  |_-> failwith "error"
let split_on_all lst = 
  List.map split_string_in_list lst
(*split on all je nas seznam "slovar", text pa nas text na katerem bomo uporabili*)
let rec value_slovar str slovar = match slovar with
    |[] -> failwith "ne najde"
    |[x; y] :: xs -> (
      if str = x then (String.sub x 0 1) ^ (y)
      else value_slovar str xs
    )
    |_ -> failwith "narobe"
let rec one_step koda slovar = 
  if String.length koda > 1 then 
    (value_slovar (String.sub koda 0 2) slovar) ^ (one_step (String.sub koda 1 ((String.length koda) - 1)) slovar)
  else(
    if String.length koda = 1 then koda
    else ""
  )
let rec number_steps koda slovar number = match number with
  |x when x > 0 -> number_steps (one_step koda slovar) slovar (number - 1)
  |0 -> koda
  |_ -> failwith "ne gre"
let rec el_in_lst el lst = match lst with
  |x :: xs -> (
    if el = x then true
    else el_in_lst el xs
  )
  |[] -> false
let rec all_diff lst new_lst = match lst with 
    |x :: xs -> (
      if (el_in_lst x new_lst) then (all_diff xs new_lst)
      else all_diff xs (x :: new_lst)
    )
    |_-> new_lst
let rec count el lst counter = match lst with
  |x :: xs -> (
    if x = el then count el xs (counter+1)
    else count el xs counter
  )
  |_-> counter
let rec count_all lst_el lst new_lst = match lst_el with
  |x :: xs -> count_all xs lst ((count x lst 0) :: new_lst)
  |_-> new_lst
let rec string_to_lst str new_lst = 
  if String.length str > 1 then string_to_lst (String.sub str 1 ((String.length str) - 1)) ((String.sub str 0 1) :: new_lst)
  else(
    if String.length str = 1 then ((String.sub str 0 1) :: new_lst)
    else new_lst
  )
let count_my lst = 
  count_all (all_diff (string_to_lst lst []) []) (string_to_lst lst []) []
let all lst = 
  count_my(number_steps (text lst) (split_on_all(slovar lst)) 10)
let max lst = 
  List.nth (List.rev (List.sort compare lst)) 0 
let min lst = 
  List.nth (List.sort compare lst) 0
let razlika_str lst = 
  ((max lst) - (min lst)) |> string_of_int
let naloga1 vsebina_datoteke = 
  let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
  let str_lst_full = (String.split_on_char '\n' vsebina_datoteke_no_spaces) in 
    razlika_str (all (str_lst_full))
(*-----------------------------------------------------------------------------------------------------*)
let naloga2 vsebina_datoteke =
  (*let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
  let str_lst = (String.split_on_char ',' vsebina_datoteke_no_spaces) in 
  let int_lst = List.map int_of_string str_lst in int_lst *)
  ""

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "data/day_14.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "out/day_14_1.out" odgovor1;
  izpisi_datoteko "out/day_14_2.out" odgovor2