let next (inChannel : in_channel) : string option =
	try
		Some (input_line inChannel)
	with End_of_file -> None



let rec nextToken (inChannel : in_channel) : Tokens.t option =
	match next inChannel with
	| Some l ->
		begin match Tokens.fromString l with
		| Some t -> Some t
		| None -> nextToken inChannel
		end
	| None -> None


let rec parse (inChannel : in_channel) : Nodes.t option =
	match parseNode inChannel with
	| Some n -> Some (parseApplication n inChannel)
	| None -> None
and parseNode (inChannel : in_channel) : Nodes.t option =
	match nextToken inChannel with
	| Some Lambda -> Some (parseAbstraction inChannel)
	| Some (Variable n) -> Some (Variable n)
	| Some LParen ->
		begin
		match parseNode inChannel with
		| Some (Abstraction (v, n)) -> Some (Abstraction (v, n))
		| Some n -> Some (parseApplication n inChannel)
		| None -> None
		end
	| _ -> None
and parseApplication (left : Nodes.t) (inChannel : in_channel) : Nodes.t =
	match parseNode inChannel with
	| Some right -> parseApplication (Application (left, right)) inChannel
	| None -> left
and parseAbstraction (inChannel : in_channel) : Nodes.t =
	let v = match nextToken inChannel with
		| Some (Variable n) -> n
		| Some t -> raise (Failure (Printf.sprintf "Unexpected %S, expected Variable" (Tokens.toString t)))
		| None -> raise (Failure "Unexpected EOF, expected Variable")
	in
	begin match nextToken inChannel with
	| Some Dot -> ()
	| Some t -> raise (Failure (Printf.sprintf "Unexpected %S, expected Dot" (Tokens.toString t)))
	| None -> raise (Failure "Unexpected EOF, expected Variable")
	end;
	match parse inChannel with
	| Some n -> Abstraction (v, n)
	| None -> raise (Failure "Expected Abstraction to contain a body")



let () : unit =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			let inChannel = open_in Sys.argv.(i)
			in
			match parse inChannel with
			| Some n -> Nodes.print n
			| None -> ()
		done
	else
		match parse stdin with
		| Some n -> Nodes.print n
		| None -> ()
