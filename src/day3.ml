let rec binary_to_decimal list_binary value = match list_binary with
  |x :: xs -> (x * int_of_float(float_of_int(2) ** float_of_int(value - 1))) :: (binary_to_decimal xs (value + 1))
  |_ -> []
let rec sum_list lst = match lst with
  |x :: xs -> x + sum_list xs
  |[] -> 0
let binary_to_decimal_sum lst value = 
  sum_list(binary_to_decimal lst value)

let rec lookup lst index zero_counter one_counter = match lst with
  (*dobi lst in na vsakem elementu lst, ki je string na izbranem indexu preveri kaj se nahaja - indexi se zacnejo na stringu z 0, zato gledamo na index - 1 *)
  |x :: xs -> (
    if String.length x < index then failwith "prekratko"
    else (
      if Char.code(String.get x (index-1)) = 49 then lookup xs index zero_counter (one_counter+1)
      else(
        if Char.code(String.get x (index-1)) = 48 then lookup xs index zero_counter (one_counter+1)
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
  | -1 -> string_of_int((binary_to_decimal_sum(List.map int_of_string gamma) 2) * (binary_to_decimal_sum(List.map int_of_string gamma) 2))
  |_-> (
    if lookup lst start_back 0 0 = "1" then (all_index lst start_back ("1" :: gamma) ("0" :: epsilon))
    else (
      if lookup lst start_back 0 0 = "0" then (all_index lst start_back ("0" :: gamma) ("1" :: epsilon))
      else failwith "ne matcha"
    )
  )
let naloga1 vsebina_datoteke = 
    (*let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
    let str_lst = (String.split_on_char '\n' vsebina_datoteke_no_spaces) in
      all_index str_lst (String.length (List.nth str_lst 0)) [] []*)
      "ups"
let naloga2 vsebina_datoteke =
  "ups"
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
