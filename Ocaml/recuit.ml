open Array
open Get_matrix
open Graphics

let scale = ref 20.
let window = " 600x600"
let window_int = 600,600
let delta_t = ref 0.001 (*pas de temps*)
let en0 = ref 1. (*énergie initiale, multiplie la vitesse initiale des points*)
let coeff = ref 1. (*145.*) (*coefficient de frottements*) (*en commentaire des valeurs expérimentales pour lesquelles ça semble pas trop mal marcher*)
let koeff = ref 20. (*8000.*) (*coefficient des ressorts*)
let wait_time = ref 0.0 



type point = {
			mutable posx : float ;
			mutable posy : float ; 
			mutable vitx : float ;
			mutable vity : float ;
			mutable x : int ; (* x,y :les positions en pixels sur l'écran pour l'affichage*)
			mutable y : int
			} 


type graph = float array array
(*représentation des graphes par une matrice*)

let random_graph n p = 
	(*n : taille du graphe à générer*)
	let m = make_matrix n n 0. in 
	for i = 0 to n-1 do 
		for j=0 to i-1 do 
			if Random.float 1. < p 
			then 
				(m.(i).(j) <- 1. ; m.(j).(i) <- 1. ) ;
		done ;
	done ;
	m
	
let random_point () = 
(*donne un point aléatoire*)
	{posx = -10. +. (Random.float 20.) ;
	posy = -10. +. (Random.float 20.) ;
	vitx = (-10. +. (Random.float 20.)) *. !en0 ;
	vity = (-10. +. (Random.float 20.)) *. !en0 ;
	x = 0 ;
	y = 0
	}

let random_vect n = 
	Array.init n (fun _ -> random_point())

let open_window () = 
	open_graph window
	
let barycentre v = 
	(*calcul le barycentre des points, utile pour centrer l'affichage du graphe*)
	let g = {posx = 0. ; posy = 0. ; vitx = 0. ; vity = 0. ; x = 0 ; y = 0} in 
	Array.iter (fun p -> g.posx <- g.posx +. p.posx ; 
						g.posy <- g.posy +. p.posy ;
						g.vitx <- g.vitx +. p.vitx ;
						g.vity <- g.vity +. p.vity) v ;
	g.posx <-  g.posx /. (float_of_int (Array.length v)) ;
	g.posy <-  g.posy /. (float_of_int (Array.length v)) ;
	g

	
let draw_graph v m = 
	(*affiche le système à un instant donné*)
	let g = barycentre v in
	for i = 0 to Array.length v -1 do 
		v.(i).x <- (int_of_float ((v.(i).posx -. g.posx) /. !scale *. (float_of_int (fst window_int)))) + (fst window_int) /2 ;
		v.(i).y <- (int_of_float ((v.(i).posy -. g.posy) /. !scale *. (float_of_int (snd window_int)))) + (snd window_int) /2 ; 
	done ; 
	set_color black ;
	for i=0 to Array.length m -1 do 
		for j = 0 to i-1 do 
			if m.(i).(j) <> 0. 
			then 
				(moveto v.(i).x v.(i).y ;
				lineto v.(j).x v.(j).y) ;
		done ;
	done ;
	set_color red ;
	for i=0 to Array.length v -1 do 
		fill_circle v.(i).x v.(i).y 5 ;
	done 

	
let distance p1 p2 = 
	(*distance au carré entre deux points *)
	(p1.posx -. p2.posx) ** 2. +. (p1.posy -. p2.posy) ** 2.

let energie v m = 
	(*énergie totale du système*)
	let e = ref 0. in 
	let n = Array.length v in 
	for i = 0 to n-1 do 
		for j = 0 to n-1 do 
				e := !e +. m.(i).(j) /. 2. *. (sqrt (distance v.(i) v.(j)) -. 1.) ** 2. ;
		done ;
	done ;
	!e
	
	
let next_step v m = 
	(*calcul les positions et vitesses à t+1*)
	(*remarque : utilisera parfois les valeurs à t ou à t+1, ne devrait pas changer grand chose*)
	Array.iteri (fun i _ -> 
		let accx = ref 0. and accy = ref 0. in 
		for j = 0 to  Array.length m -1 do 
			let d = sqrt (distance v.(i) v.(j)) in 
			if d <> 0. then 
			(
				accx := (!accx) +. (v.(j).posx -. v.(i).posx) /. d *. (d -. 1.) *. (!koeff) *. m.(i).(j) ; 
				accy := (!accy) +. (v.(j).posy -. v.(i).posy) /. d *. (d -. 1.) *. (!koeff) *. m.(i).(j) ; 
			)
		done ;
		(*on ajoute un coefficient de frottements*)
		accx := !accx -. !coeff *. v.(i).vitx ;
		accy := !accy -. !coeff *. v.(i).vity ;
		v.(i).posx <- v.(i).posx +. !delta_t *. v.(i).vitx ;
		v.(i).posy <- v.(i).posy +. !delta_t *. v.(i).vity ;
		v.(i).vitx <- v.(i).vitx +. !delta_t *. !accx ;
		v.(i).vity <- v.(i).vity +. !delta_t *. !accy ;
	) v

let keyboard_event v = 
(*interface avec le clavier*)
		if key_pressed () then 
			begin 
			match read_key() with 
				| '+' -> scale := !scale /. 1.5 
				| '-' -> scale := !scale *. 1.5 
				| 'q' -> exit 0
				| '8' -> coeff := !coeff *. 1.5 
				| '2' -> coeff := !coeff /. 1.5 
				| '6' -> koeff := !koeff *. 1.5 
				| '4' -> koeff := !koeff *. 1.5 
				| 'r' ->  Array.iteri (fun i _ -> v.(i) <- random_point ()) v
				| 'd' -> Format.printf "friction : %f\nspring : %f@." (!coeff) (!koeff)
				| 'h' -> Format.printf "keyboard :\n'h': diplay help \n'q': exit\n'-'/'+' : zoom\n'4'/'6': spring coefficient \n'2'/'8' : friction ceofficient\n'r': restart process\n'd': diplay coeffs@."
				| _ -> ()
			end

	
	
let main () = 
	let f = ref "" in 
	let emin = ref infinity in
	Arg.parse [] (fun s -> f:= s) "usage : recuit.exe <filename>\n" ;
	let m = read_graph !f in 
	let v = random_vect (Array.length m) in 
	open_window() ;
	Format.printf "type 'h' on the graphic window for help@." ;
	auto_synchronize false ;
	while true do 
		keyboard_event v ;
		clear_graph() ;
		draw_graph v m ;
		moveto 0 0 ;
		let e = energie v m in 
		if !emin > e then emin := e ;
		draw_string (Format.sprintf "E = %f   Emin = %f" e !emin) ;
		next_step v m ;
		synchronize () ;
		Thread.delay !wait_time ; (*dans le cas où on voudrait ralentir l'animation, mis à zéro par défault*)
	done 
			
				
	
let () = main()
