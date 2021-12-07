let rec list_no_new_lines lst = match lst with
  |x :: xs -> (
    if x = "" then list_no_new_lines xs 
    else x :: (list_no_new_lines xs)
  )
  |[] -> []
let other lst = match lst with
|x :: xs -> xs
|_-> []
let random lst = match lst with
|x :: xs -> x
|_-> ""
let split_random lst = String.split_on_char ',' lst
let split_other_5 lst = match lst with
| x:: y:: z :: w :: v :: xs -> [x; y ; z; w; v] :: []
|_->[]
let rec split_lst_of_lst lst_of_lst index = match lst_of_lst with
| x :: xs -> List.nth x (index-1) :: (split_lst_of_lst xs index)
|_-> []
let rec split_on_index_list_of_list lst value = 
  if value <= List.length(List.nth lst 0) then (split_lst_of_lst lst value) :: [split_lst_of_lst lst (value+1)]
  else []
let rec split_lst_of_lst_all_index lst = 
  split_on_index_list_of_list lst 0
let rec split_boards lst = 
  (split_other_5 (other lst)) :: [split_lst_of_lst_all_index (split_other_5 (other lst))]
let rec get_random lst =
  split_random (random lst)

(*split_boards nam iz zacetnih podatkov naredi seznam boardov, ki so seznami [[[vrstice]; [stolpci]];[board]]*)
(*get_random nam iz zacetnik podatkov da seznam vlecenja stevil (v stringih)*)

let naloga1 vsebina_datoteke = 
  let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
  let str_lst = (String.split_on_char '\n' vsebina_datoteke_no_spaces) in 
  List.nth str_lst 0
let naloga2 vsebina_datoteke =
  let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
  let str_lst = (String.split_on_char '\n' vsebina_datoteke_no_spaces) in 
  List.nth str_lst 0


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
  let vsebina_datoteke = preberi_datoteko "data/day_4.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "out/day_4_1.out" odgovor1;
  izpisi_datoteko "out/day_4_2.out" odgovor2