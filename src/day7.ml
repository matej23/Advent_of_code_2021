let absolut a = 
  if a >= 0 then a
  else -a
let mediana sorted_lst = 
  (*nth da index -1 element*)
  [
    (List.nth sorted_lst ((List.length sorted_lst) / 2));
    (List.nth sorted_lst ((List.length sorted_lst) / 2) + 1);
  ]
let rec difference number lst = match lst with 
  |[] -> 0
  |x :: xs -> absolut (x - number) + difference number xs

let rec better lst_numbers lst = match lst_numbers with
  |x :: y :: [] -> (
    if (difference x lst) < (difference y lst) then (difference x lst)
    else (difference y lst)
  )
  |_ -> failwith "neki je narobe"
let naloga1 vsebina_datoteke = 
  let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
  let str_lst = (String.split_on_char ',' vsebina_datoteke_no_spaces) in 
  let int_lst = List.map int_of_string str_lst in 
  let sorted_lst = List.sort compare int_lst in 
    better (mediana sorted_lst) sorted_lst |> string_of_int

(*-----------------------------------------------------------------------------------------------------*)
let rec least lst =
	   match lst with
	   | [] -> 0
	   | x :: [] -> x
	   | x :: xs -> 
	         let v = least xs in
	         if x < v then
	            x
	         else
            v
let rec biggest lst =
  match lst with
  | [] -> 0
  | x :: [] -> x
  | x :: xs -> 
          let v = biggest xs in
        if x > v then
             x
        else
           v
let rec range i j = if i > j then [] else i :: (range (i+1) j)
let all_possible lst = 
  range (least lst) (biggest lst)

let rec difference_2 n = match n with
  |1 -> 1
  |x when x > 1 -> x + difference_2 (n-1)
  |_ -> 0
let abs a = 
  if a >= 0 then a
  else -a
let rec final_difference lst number = match lst with
| [] -> 0
| x ::xs -> difference_2 (abs (x - number)) + final_difference xs number
let all_differences lst = 
  (List.map (final_difference lst)) (all_possible lst)
let naloga2 vsebina_datoteke =
  let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
  let str_lst = (String.split_on_char ',' vsebina_datoteke_no_spaces) in 
  let int_lst = List.map int_of_string str_lst in 
    (all_differences (int_lst)) |> least |> string_of_int

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
  let vsebina_datoteke = preberi_datoteko "data/day_7.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "out/day_7_1.out" odgovor1;
  izpisi_datoteko "out/day_7_2.out" odgovor2