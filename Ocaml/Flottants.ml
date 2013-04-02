type point = {x : float ; y : float}

let poly a b c d = 
	let mc = 1. +. ((a.y -. c.y)/.(c.x -. a.x))**2. in 
	let r1 =  (a.x -. b.x) **2. +. (a.y -. b.y) ** 2.in 
	let r2 = (c.x -. d.x) **2. +. (c.y -. d.y) ** 2.in 
	let midc = -. a.y +. ((a.y -. c.y)/.(c.x -. a.x)) *. (-. a.x +. ((r1 -.r2) -. a.x **2. -. a.y ** 2. +. c.x ** 2. +. c.y **2. ) /. (2. *. (c.x -. a.x))) in 
	let c = a.y ** 2. -. r1 +. ((-. a.x +. ((r1 -.r2) -. a.x **2. -. a.y ** 2. +. c.x ** 2. +. c.y **2. ) /. (2. *. (c.x -. a.x)))) ** 2. in 
	[mc,midc,c]