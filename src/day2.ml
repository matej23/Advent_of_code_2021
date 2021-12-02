let naloga1 vsebina_datoteke = 
    let lst_str_tuple = (String.split_on_char '\n' vsebina_datoteke) in 
        let lst_tuple = List.map (String.split_on_char ' ') lst_str_tuple in 
        let rec position depth horizontal lst_tuple = match lst_tuple with
            (*[["neki"; "10"]; ["nekidruzga"; 11]]*)
            |first :: xs -> (match first with
                |way :: value :: other ->
                    if way = "forward" then position depth (horizontal + (int_of_string value)) xs
                    else (
                        if way = "down" then position (depth + (int_of_string value)) horizontal xs
                        else (
                            if way = "up" then position (depth - (int_of_string value)) horizontal xs
                            else failwith "kam pa kam"
                        )
                    )
                |_ -> depth * horizontal
            )
            |_-> horizontal * depth
        in position 0 0 lst_tuple |> string_of_int
let naloga2 vsebina_datoteke =
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
  let vsebina_datoteke = preberi_datoteko "data/day_2.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "out/day_2_1.out" odgovor1;
  izpisi_datoteko "out/day_2_2.out" odgovor2