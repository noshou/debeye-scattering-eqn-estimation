(* represents an atom at coordinate xyz *)
type atom = {
    coords: Data_loading.point; 
    name: string; 
    form_fact: float -> Complex.t
}

(* public constructor for atom *)
let create point_ name_ =
    {   coords = point_; 
        name = name_; 
        form_fact = 
            fun q -> Form_fact.form_fact q name_
    }

(* public facing accessors *)
let coords a = a.coords
let name a = a.name
let form_fact a q = a.form_fact q