type voiture = {mutable vitesse: int; mutable destination : int}
type state = Empty | Full of voiture
type graph = {
	vtx : state array;
	edges : (int*int option) list array;
}
