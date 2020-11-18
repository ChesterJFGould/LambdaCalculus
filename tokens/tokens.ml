type t =
	| Lambda
	| Variable of string
	| LParen
	| RParen
	| Dot
	
let toString (token : t) : string =
	match token with
	| Lambda -> "Lambda"
	| Variable n -> "Variable " ^ n
	| LParen -> "LParen"
	| RParen -> "RParen"
	| Dot -> "Dot"
	
let fromString (s : string) : t option =
	match String.split_on_char ' ' s with
	| ["Lambda"] -> Some Lambda
	| ["Variable"; n] -> Some (Variable n)
	| ["LParen"] -> Some LParen
	| ["RParen"] -> Some RParen
	| ["Dot"] -> Some Dot
	| _ -> None
