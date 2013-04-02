{
let n = ref 0 
}

let bin = ['0' - '1']
let entier = ['0' - '9'] +
let space = (' ' | '\t' )* 
let newline = ('\n' | '\r' '\n')

rule header = parse 
	| space (entier as s) space newline {
							n := int_of_string s ; 
							let m = Array.make_matrix (!n) (!n) 0. in 
							get_matrix m 0 lexbuf
							}
	| _ {Format.printf "header : sytax error@." ; exit 1 }
							
and get_matrix m i = parse
	| (bin+ as s) space (newline)* {	 
							if (String.length s <> !n) 
							then (Format.printf "incorrect number of rows line %d@." (i+2) ; exit 1) ;
							for j = 0 to !n -1 do 
								if s.[j] = '1' 
								then 
									(m.(i).(j) <- 1. ; m.(j).(i) <- 1.) ;
							done ;
							if (i = !n -1) 
							then m 
							else (get_matrix m (i+1) lexbuf)
							}
	| eof {Format.printf "get_matrix : premature end of file@." ; exit 1 }
	| _ {Format.printf "get_matrix : Syntax error line %d@." (i+2) ; exit 1}
	
	
{
let read_graph s = 
	let lexbuf = Lexing.from_channel (open_in s) in 
	header lexbuf 
}
