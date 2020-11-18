type t =
	| Variable of string
	| Abstraction of string * t
	| Application of t * t



let rec print (node : t) : unit =
	match node with
	| Variable n -> print_endline ("Variable " ^ n)
	| Abstraction (v, n) ->
		begin
		print_endline ("Abstraction " ^ v);
		print n;
		end
	| Application (left, right) ->
		begin
		print_endline "Application";
		print left;
		print right;
		end

let rec fromChannel (inChannel : in_channel) : t =
	match String.split_on_char ' ' (input_line inChannel) with
	| ["Variable"; n] -> Variable n
	| ["Abstraction"; v] -> Abstraction (v, (fromChannel inChannel))
	| ["Application"] -> 
		let left = fromChannel inChannel in
		let right = fromChannel inChannel in
		Application (left, right)
	| l -> raise (Failure (Printf.sprintf "Unexpected %S" (String.concat " " l)))



let printTree (node : t) : unit =
	let rec printIndent (node : t) (indent : string) (last : bool) : unit =
		print_string indent;
		match node with
		| Variable n ->
			if last then
				begin
				print_string "\\-";
				print_endline n
				end
			else
				begin
				print_string "|-";
				print_endline n;
				end
		| Abstraction (v, n) ->
			if last then
				begin
				print_string "\\-";
				print_endline ("λ" ^ v);
				printIndent n (indent ^ "  ") true;
				end
			else
				begin
				print_string "|-";
				print_endline ("λ" ^ v);
				printIndent n (indent ^ "| ") true;
				end
		| Application (left, right) ->
			if last then
				begin
				print_string "\\-";
				print_endline "@";
				printIndent left (indent ^ "  ") false;
				printIndent right (indent ^ "  ") true;
				end
			else
				begin
				print_string "|-";
				print_endline "@";
				printIndent left (indent ^ "| ") false;
				printIndent right (indent ^ "| ") true;
				end
	in
	printIndent node "" true


let rec toString (node : t) : string =
	match node with
	| Variable n -> n
	| Abstraction (v, n) ->"(λ" ^ v ^ ". " ^ (toString n) ^ ")"
	| Application (left, right) -> (toString left) ^ (toString right)
