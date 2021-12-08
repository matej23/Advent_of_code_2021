let rec split_on str_lst_full = match str_lst_full with
  |[] -> []
  |x :: xs -> List.map String.trim (String.split_on_char '|' x) :: split_on xs
let rec get_substrings lst_of_lst_str new_list = match lst_of_lst_str with
  |x :: xs -> 
    get_substrings xs((String.split_on_char ' ' (List.nth x 1)) @ new_list)
  |_ -> new_list
let all_at_once str_lst_full = 
  get_substrings (split_on str_lst_full) []

let rec count_if str_lst times = match str_lst with
  |[] -> times
  |x :: xs -> (
    if (String.length x = 2 || String.length x  = 3 || String.length x = 4 || String.length x = 7) then 
      count_if xs (times +1)
    else count_if xs times 
  )
let naloga1 vsebina_datoteke = 
  let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
  let str_lst_full = (String.split_on_char '\n' vsebina_datoteke_no_spaces)
  (*nam da ["nskwn qkjnf | kan snj"]*)
  (*"dac abcf ac fdbcga dgcbae gcbfde fgcbd agfed adcgf cdbgfea | cbfa bcafdg cbfa bafcgd\nabfgd baedc feb fgde fbagcd facbdge agcbfe bgafde fe ebdfa | bef fe dgfe feb"*)
  in (count_if (all_at_once str_lst_full) 0) |> string_of_int

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
  let vsebina_datoteke = preberi_datoteko "data/day_8.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "out/day_8_1.out" odgovor1;
  izpisi_datoteko "out/day_8_2.out" odgovor2