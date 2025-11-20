exception Malformed_xyzEntry of string
exception Malformed_xyzFile of string

(* each data in xyz file is tabulated as 
|element | x-coord | y-coord| z-coord| 
where the first two lines are non-data info/comments *)

(* matches beginning of file *)
let re1 a= Str.regexp "^[ \t]*"

(* matches some name string *)
let re2 = Str.regexp "\\([A-Za-z]+\\)"

(* matches some coordinate value *)
let re3 = Str.regexp "\\(-?[0-9]+\\(\\.[0-9]+\\)?\\)"

(* matches end of regex *)
let re4 = Str.regexp "[ \t]*$"

(* matches tab delim files (tsv) *)
let re_tsv = 
    Str.regexp (re1 ^ re2 ^ "[\t]" ^ re3  ^ "[\t]" ^ re3  ^ "[\t]" ^ re3  ^ re4)

(* matches comma delim files (csv) *)
let re_csv = 
    Str.regexp (re1 ^ re2 ^ "[ \t]*,[ \t]*" ^ re3  ^ "[ \t]*,[ \t]*" ^ re3  ^ "[ \t]*,[ \t]*" ^ re3  ^ re4)

(** loads an xyz file (tsv or csv) from a file path.
Note: the list is returned backwards w.r.t how the file was read *)
let load_xyz fp =

    (* initialize mutable empty list of atoms *)
    let atoms = ref [] in 

    (* read file and skip first 2 lines of xyz file*)
    let f = open_in fp in 
    let _ = input_line f in 
    let _ = input_line f in 

    (* first row parsing *)
    let row = input_line f in 

    (* detect delim type*)
    let _delimType = 
        if (Str.string_match re_tsv row 0) then 
            re_tsv
        else if (Str.string_match re_csv row 0) then 
            re_csv 
        else 
            raise (Malformed_xyzFile "xyz file must be csv or tsv!")
    in

    (* error message *)
    let errmsg m = match _delimType with
        | re_tsv -> "Expected tsv; got:" ^ m
        | re_csv -> "Expected csv; got:" ^ m
    in 

    (* parse data from first row *)
    let n = Str.matched_group 1 row in 
    let p = 
        {   x = float_of_string (Str.matched_group 2 row);
            y = float_of_string (Str.matched_group 3 row);
            z = float_of_string (Str.matched_group 4 row);
        }
    in 

    atoms := (Atom.create n p) :: !atoms;

    (* parse rest of file *)
    try 
        while true do 
            let row = input_line f in 
            if (Str.string_match _delimType row 0) then 
                let n = Str.matched_group 1 row in 
                let p = 
                    {   x = float_of_string (Str.matched_group 2 row);
                        y = float_of_string (Str.matched_group 3 row);
                        z = float_of_string (Str.matched_group 4 row);
                    }
                in 
                atoms := (Atom.create n p) :: !atoms;
            else 
                raise (Malformed_xyzEntry row)
        done;
        !atoms; (* never reached (since end of file always hit) but needed for type *)
    with 
        | End_of_file -> close_in f; !atoms 
        | e -> close_in_noerr f; raise e
