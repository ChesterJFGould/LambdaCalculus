let next (inChannel : in_channel) : char option =
	try
		Some (input_char inChannel)
	with End_of_file -> None



let rec lex (inChannel : in_channel) : unit =
	let variableCharset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
	in
	let rec tokenize () : Tokens.t option =
		match next inChannel with
		| Some '\\' -> Some Lambda
		| Some '(' -> Some LParen
		| Some ')' -> Some RParen
		| Some '.' -> Some Dot
		| Some c ->
			if String.contains variableCharset c then
				Some (Variable (String.make 1 c))
			else
				tokenize ()
		| None -> None
	in
	match tokenize () with
	| Some t ->
		print_endline (Tokens.toString t);
		lex inChannel
	| None -> ()



let () : unit =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			let inChannel = open_in Sys.argv.(i)
			in
			lex inChannel
		done
	else
		lex stdin
