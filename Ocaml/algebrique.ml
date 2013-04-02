type ratio = {nume : int ; denum : int}

let rec pgcd a b = match a,b with
	| 0,a -> a
	| a,0 -> a 
	| a,b ->  pgcd b (a mod b)

let simplify_rat q = 
	let d = pgcd q.nume q.denum in 
	{nume = q.nume / d ; denum = q.denum / d}

let add_rat q1 q2 = 
	simplify_rat ({nume = q1.nume * q2.denum + q2.nume * q1.denum; denum = q1.denum*q2.denum})
	
let minus_rat q1 = 
	{nume = - q1.nume ; denum = q1.denum}
	
let minus_rat q1 q2 = 
	add_rat q1 (minus_rat q2)
	
let inv_rat q1 = 
	{nume = q1.denum ; denum = q1.nume}
	
let mult_rat q1 q2 = 
	simplify_rat {nume = q1.nume * q2.nume ; denum = q1.denum * q2.denum} 

let div_rat q1 q2 = mult_rat q1 (inv_rat q2)

let square_int a = a*a 

let is_sqr q = (square_int (int_of_float (sqrt (float_of_int q.nume))) = q.nume  ) && (square_int (int_of_float (sqrt (float_of_int q.denum))) = q.denum)
	
let sqrt_rat q = 
	{nume = int_of_float (sqrt (float_of_int q.nume)) ; denum = int_of_float (sqrt (float_of_int q.denum)) }
	
let zero_rat = {nume = 0 ; denum = 1}

	
type alg = ratio list

let rec simplify_alg = function 
	| [] -> [] 
	| t::q -> let q = simplify_alg q in 
			if List.length q = 1 
			then 
				let b = List.hd q in 
				if is_sqr b 
					then 
					[add_rat t (sqrt_rat b)]
					else
					t::q
			else 
				t::q


let rec add_alg l1 l2 = match l1,l2 with
	| [],_ -> l2 
	| _,[] -> l1 
	| (t1::q1),(t2::q2) -> (add_rat t1 t2)::(add_alg (add_alg q1 q2) ((zero_rat)::(mult_alg q1 q2)))
	
and mult_alg l1 l2 = match l1,l2 with  
	| [],_ -> [] 
	| _,[] -> []
	| [a],(t::q) | (t::q),[a] -> (mult_rat t a) :: (mult_alg [mult_rat a a] q)
	| (t::q),(t'::q') -> add_alg 
							(add_alg [mult_rat t t'] (mult_alg [t] (zero_rat::q')))  
							(add_alg (mult_alg [t'] (zero_rat::q)) (zero_rat::(mult_alg q q')))

(*							
let inv_alg = function 
	| [] -> failwith "inv_alg : division by zero"
	| [a] -> [inv_rat a]
	| t::q -> 
	
	*)
	
let _ = add_alg [zero_rat;{nume  = 2 ; denum = 1}] [zero_rat ; {nume = 2 ; denum = 1}]
