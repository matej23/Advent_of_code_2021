let digits n =
  let rec loop n acc =
      if n = 0 then acc
      else loop (n/10) (n mod 10::acc) in
  match n with
  | 0 -> [0]
  | _ -> loop n []
let rec vrstice str = 
  List.map digits(List.map int_of_string (String.split_on_char '\n' (String.trim str))) 
let rec range i j = if i > j then [] else i :: (range (i+1) j)
let notranji_desno_levo vrstice = 
  range 1 (List.length (List.nth vrstice 0 ) - 2)
let notranji_gor_dol vrstice = 
  range 1 ((List.length vrstice) - 1
(*
let rec stolpec lst_vrstice index = match lst_vrstice with
  |[] -> []
  |x :: xs -> ((List.nth x (index-1)) :: (stolpec xs index))
let rec range i j = if i > j then [] else i :: (range (i+1) j)
let rec stolpci lst_vrstice indexs = match indexs with
  |[] -> []
  |x :: xs -> (stolpec lst_vrstice x) :: (stolpci lst_vrstice xs)
let vsi_stolpci lst_vrstice = 
  stolpci lst_vrstice (range 1 (List.length (List.nth lst_vrstice 0 )))
(*uspesn dobimo vse vrstice v obliki seznama seznamov in vse stolpce obliki seznama seznamov*)
*)
(*------------------------------------------------------------------------------------*)
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