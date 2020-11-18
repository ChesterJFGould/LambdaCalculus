let rec substitute (sub : Nodes.t) (var : string) (node : Nodes.t) : Nodes.t =
	match node with
	| Variable n ->
		if n = var then
			sub
		else
			Variable n
	| Abstraction (v, n) ->
		if v = var then
			Abstraction (v, n)
		else
			Abstraction (v, (substitute sub var n))
	| Application (left, right) ->
		Application ((substitute sub var left), (substitute sub var right))



let rec betaReduce (node : Nodes.t) : Nodes.t =
	match node with
	| Application (left, right) ->
		begin
		match betaReduce left with
		| Abstraction (v, n) -> betaReduce (substitute (betaReduce right) v n)
		| _ -> Application (left, right)
		end
	| n -> n



let () : unit =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			let inChannel = open_in Sys.argv.(i)
			in
			print_endline (Nodes.toString (betaReduce (Nodes.fromChannel inChannel)))
		done
	else
		print_endline (Nodes.toString (betaReduce (Nodes.fromChannel stdin)))
