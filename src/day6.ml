(*majsa stevila od 80, ki jih lahko napisemo kot vsoto 7 in 9, pri cemer je 9-ka uporabljena najvec tolikokrat kot 80*)
(*80 -> n, n + 7, n + 9, ... 7 9 , 7 7 7, 7 7 9 , 7 7 7 7 , 7 7 7 9, 7 7 9 9, 7 7 7 7 7, 7 7 7 7 9, 7 7 7 9 9, 7 7 7 7 7 7; 7 7 7 7 7 9; 7 7 7 7 9 9 ; 7 7 7 9 9 9  *)
(*kako vse se da razcepiti stevilo sedaj kot vsoto 7 in 9, pri cemer z vsoto enih od stevil *)
let rec sum_lst lst = match lst with 
  |[] -> 0
  |x :: xs -> (x + (sum_lst xs))

let rec max_value lst start = match lst with
| [] -> start
| x :: xs -> (
  if start <= x then max_value xs xs
  else max_value xs start
)

let rec possible_numbers lst = match lst with
  |[] -> []
  |x :: xs -> (
    if (max_value lst 0) > 85 then lst
    else (x + 7) :: (x + 9) :: possible_numbers xs
  )

let naloga1 vsebina_datoteke = 
  let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
  let str_lst = (String.split_on_char ',' vsebina_datoteke_no_spaces) in 
  let int_lst = List.map int_of_string str_lst in 
    (List.length (add_many_days int_lst 80)) |> string_of_int

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
  let vsebina_datoteke = preberi_datoteko "data/day_6.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "out/day_6_1.out" odgovor1;
  izpisi_datoteko "out/day_6_2.out" odgovor2