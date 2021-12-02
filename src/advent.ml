let naloga1 vsebina_datoteke = 
    let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
    let str_lst = (String.split_on_char '\n' vsebina_datoteke_no_spaces) in 
        let lst = List.map int_of_string str_lst in 
                let rec increase lst counter = 
                    match lst with
                    |first :: second :: xs -> (
                        if first < second then increase (second :: xs) counter + 1
                        else increase (second :: xs) counter 
                    )
                    |_ -> counter 
    in increase lst 0 |> string_of_int
let naloga2 vsebina_datoteke =
    let vsebina_datoteke_no_spaces = String.trim vsebina_datoteke in 
    let str_lst = (String.split_on_char '\n' vsebina_datoteke_no_spaces) in 
        let lst = List.map int_of_string str_lst in 
                let rec increase_triple lst counter before = 
                    match lst with
                    |first :: second :: third :: xs -> (
                        if before != 0 then (
                            if before < (first + second + third) then increase_triple (second :: third :: xs) (counter + 1) (first + second + third)
                            else increase_triple (second :: third :: xs) (counter) (first + second + third)
                        )
                        else let before = (first + second + third)
                            in increase_triple (second :: third :: xs) counter before
                    )
                    |_ -> counter 
    in increase_triple lst 0 0 |> string_of_int

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
    let vsebina_datoteke = preberi_datoteko "data/day_0.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "out/day_1_1.out" odgovor1;
    izpisi_datoteko "out/day_1_2.out" odgovor2
