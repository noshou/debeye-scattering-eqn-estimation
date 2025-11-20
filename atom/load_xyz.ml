exception Malformed_xyzEntry of string
exception Malformed_xyzFile of string


(* below does not work :( *) 

(*
(* matches beginning of file *)
let re1 = "^[ \t]*"

(* matches some name string *)
let re2 = "\\([A-Za-z]+\\)"

(* matches a coordinate value (integer or decimal, possibly negative) *)
let re3 = "\\(-?[0-9]+\\(?:\\.[0-9]+\\)?\\)"

(* matches end of line *)
let re4 = "[ \t]*$"

(* matches tab/space-delimited files (TSV) *)
let re_tsv = 
  Str.regexp (re1 ^ re2 ^ "[ \t]+" ^ re3 ^ "[ \t]+" ^ re3 ^ "[ \t]+" ^ re3 ^ re4)

(* matches comma-delimited files (CSV) *)
let re_csv = 
  Str.regexp (re1 ^ re2 ^ "[ \t]*,[ \t]*" ^ re3 ^ "[ \t]*,[ \t]*" ^ re3 ^ "[ \t]*,[ \t]*" ^ re3 ^ re4) *)

(** loads an xyz file (tsv or csv) from a file path.
Note: the list is returned backwards w.r.t how the file was read 
*)


let load_xyz fp =

    (* initialize mutable empty list of atoms *)
    let atoms = ref [] in 

    (* read file and skip first 2 lines of xyz file*)
    let f = open_in fp in 
    let _ = input_line f in 
    let _ = input_line f in 

    (* first row parsing *)
    let row = input_line f in 
    print_endline row;
    (* detect delim type*)
    let _delimType =
        if Str.string_match re_tsv row 0 then
            re_tsv
        (* else if Str.string_match re_csv row 0 then
            re_csv *)
        else
            raise (Malformed_xyzFile "xyz file must be csv or tsv!")
    in

    (* error message *)
    let errmsg m =
        if _delimType == re_tsv then
            "Expected tsv; got: " ^ m
        (* else if _delimType == re_csv then
            "Expected csv; got: " ^ m *)
        else
            "Unknown delimiter; got: " ^ m
    in
    (* parse data from first row *)
    let n = Str.matched_group 1 row in 
    let p : Atom_.coord = 
        {   x = float_of_string (Str.matched_group 2 row);
            y = float_of_string (Str.matched_group 3 row);
            z = float_of_string (Str.matched_group 4 row);
        }
    in 

    atoms := (Atom_.create_atom p n) :: !atoms;

    (* parse rest of file *)
    try 
        while true do 
            let row = input_line f in 
            if (Str.string_match _delimType row 0) then 
                let n = Str.matched_group 1 row in 
                let p : Atom_.coord = 
                    {   x = float_of_string (Str.matched_group 2 row);
                        y = float_of_string (Str.matched_group 3 row);
                        z = float_of_string (Str.matched_group 4 row);
                    }
                in 
                atoms := (Atom_.create_atom p n) :: !atoms;
            else 
                raise (Malformed_xyzEntry (errmsg row))
        done;
        !atoms; (* never reached (since end of file always hit) but needed for type *)
    with 
        | End_of_file -> close_in f; !atoms 
        | e -> close_in_noerr f; raise e
