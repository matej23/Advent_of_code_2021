let rec binary_to_decimal list_binary_str =
  int_of_string("0b" ^ String.concat "" list_binary_str)

let rec lookup lst index zero_counter one_counter = match lst with
  (*dobi lst in na vsakem elementu lst, ki je string na izbranem indexu preveri kaj se nahaja - indexi se zacnejo na stringu z 0, zato gledamo na index - 1 *)
  |x :: xs -> (
    if String.length x < index then failwith "prekratko"
    else (
      if Char.code(String.get x (index -1 )) = 49 then lookup xs index zero_counter (one_counter+1)
      else(
        if Char.code(String.get x (index -1 )) = 48 then lookup xs index (zero_counter +1) one_counter
        else failwith "ne prepozna"
      )
    )
  )
  |[] -> (
    if zero_counter > one_counter then "0"
    else (
      if zero_counter < one_counter then "1"
      else failwith "neki je narobe"
    )
  )
let rec all_index lst start_back gamma epsilon = match start_back with
  | 0 -> string_of_int(
    (binary_to_decimal(gamma) * binary_to_decimal(epsilon))
    )
  |_-> (
    if lookup lst start_back 0 0 = "1" then (all_index lst (start_back -1) ("1" :: gamma) ("0" :: epsilon))
    else (
      if lookup lst start_back 0 0 = "0" then (all_index lst (start_back-1) ("0" :: gamma) ("1" :: epsilon))
      else failwith "ne matcha"
    )
  )
let naloga1 vsebina_datoteke = 
    let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
    let str_lst = (String.split_on_char '\n' vsebina_datoteke_no_spaces) in
      all_index str_lst (String.length (List.nth str_lst 0)) [] []
(*-------------------------------------------------------------------------------------------------------------------*)
  let rec lookup_2_gamma lst index zero_counter one_counter = match lst with
  (*dobi lst in na vsakem elementu lst, ki je string na izbranem indexu preveri kaj se nahaja - indexi se zacnejo na stringu z 0, zato gledamo na index - 1 *)
  |x :: xs -> (
    if String.length x < index then failwith "prekratko"
    else (
      if Char.code(String.get x index) = 49 then lookup_2_gamma xs index zero_counter (one_counter+1)
      else(
        if Char.code(String.get x index) = 48 then lookup_2_gamma xs index (zero_counter +1) one_counter
        else failwith "ne prepozna"
      )
    )
  )
  |[] -> (
    if zero_counter > one_counter then "48"
    else ("49"
    )
  )
  (*prvi element lst ima indeks 1*)
let rec lookup_2_epsilon lst index zero_counter one_counter = match lst with
  (*dobi lst in na vsakem elementu lst, ki je string na izbranem indexu preveri kaj se nahaja - indexi se zacnejo na stringu z 0, zato gledamo na index - 1 *)
  |x :: xs -> (
    if String.length x < index then failwith "prekratko"
    else (
      if Char.code(String.get x index) = 49 then lookup_2_epsilon xs index zero_counter (one_counter+1)
      else(
        if Char.code(String.get x index) = 48 then lookup_2_epsilon xs index (zero_counter +1) one_counter
        else failwith "ne prepozna"
      )
    )
  )
  |[] -> (
    if one_counter > zero_counter then "49"
    else ("48"
    )
  )
  (*prvi element lst ima indeks 1*)
  let rec remove lst index based_value = match lst with
  |x :: xs -> (
    if Char.code(String.get x index) = based_value then (x :: remove xs index based_value)
    else (remove xs index based_value)
    )
  |_ -> []
(*string.get steje prvi element kot 0 indeks*)
let rec all_index_2_epsilon lst start_front = 
  if start_front < String.length(List.nth lst 0) then (
    all_index_2_epsilon (remove lst start_front (int_of_string(lookup_2_epsilon lst start_front 0 0))) (start_front + 1)
  )
  else lst
  (*list.nth steje prvi element kot 0, string.length pa steje dolzino od 1 naprej*)
let rec all_index_2_gamma lst start_front = 
  if start_front < String.length(List.nth lst 0) then (
  all_index_2_gamma (remove lst start_front (int_of_string(lookup_2_gamma lst start_front 0 0))) (start_front + 1)
  )
  else lst
let rec binary_to_decimal list_binary_str =
  int_of_string("0b" ^ String.concat "" list_binary_str)
  let find_gamma_epsilon lst =
    string_of_int(binary_to_decimal(all_index_2_epsilon lst 0) * binary_to_decimal(all_index_2_gamma lst 0))
  let naloga2 vsebina_datoteke =
    let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
    let str_lst = (String.split_on_char '\n' vsebina_datoteke_no_spaces) in 
    find_gamma_epsilon str_lst

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
  let vsebina_datoteke = preberi_datoteko "data/day_3.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "out/day_3_1.out" odgovor1;
  izpisi_datoteko "out/day_3_2.out" odgovor2
