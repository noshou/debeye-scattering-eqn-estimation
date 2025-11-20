(* module for internal use only; re-exported in atom.ml *)

(* represents an <x,y,z> coordinate *)
type coord = {x: float; y: float; z: float}

(* represents an atom at coordinate xyz; internal to atom module *)
type atom = {
    xyz: coord; 
    name: string; 
    form_fact: float -> Complex.t
}

(* public constructor for atom *)
let create_atom p n =
    {   xyz = p; 
        name = n; 
        form_fact = 
            fun q -> Form_fact.form_fact q n
    }

(* public facing accessors *)
let xyz a = a.xyz
let name a = a.name
let form_fact a q = a.form_fact q
