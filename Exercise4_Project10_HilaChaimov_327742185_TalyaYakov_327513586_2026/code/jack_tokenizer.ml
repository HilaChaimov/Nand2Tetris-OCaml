type token_type =
  | Keyword
  | Symbol
  | Identifier
  | IntConst
  | StringConst

let keywords = [
  "class"; "constructor"; "function"; "method";
  "field"; "static"; "var";
  "int"; "char"; "boolean"; "void";
  "true"; "false"; "null"; "this";
  "let"; "do"; "if"; "else"; "while"; "return"
]

let symbols = [
  '{'; '}'; '('; ')'; '['; ']';
  '.'; ','; ';';
  '+'; '-'; '*'; '/';
  '&'; '|'; '<'; '>'; '='; '~'
]



let is_symbol c =
  List.mem c symbols

let is_digit c =
  c >= '0' && c <= '9'

let is_letter c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  c = '_'

let is_identifier_char c =
  is_letter c || is_digit c

let is_space c =
  c = ' ' || c = '\t' || c = '\r' || c = '\n'

let is_keyword s =
  List.mem s keywords


let is_string_token token =
  String.length token >= 2 &&
  token.[0] = '"' &&
  token.[String.length token - 1] = '"'

let remove_quotes token =
  String.sub token 1 (String.length token - 2)

let escape_xml s =
  match s with
  | "<" -> "&lt;"
  | ">" -> "&gt;"
  | "&" -> "&amp;"
  | "\"" -> "&quot;"
  | _ -> s

let string_of_token_type = function
  | Keyword -> "keyword"
  | Symbol -> "symbol"
  | Identifier -> "identifier"
  | IntConst -> "integerConstant"
  | StringConst -> "stringConstant"

let token_type token =
  if is_keyword token then
    Keyword
  else if String.length token = 1 && is_symbol token.[0] then
    Symbol
  else if String.for_all is_digit token then
    IntConst
  else
    Identifier

let write_token oc token =
  let ttype = token_type token in
  let tag = string_of_token_type ttype in
  let value = escape_xml token in
  output_string oc ("<" ^ tag ^ "> " ^ value ^ " </" ^ tag ^ ">\n")

let write_string_token oc str =
  let value = escape_xml str in
  output_string oc ("<stringConstant> " ^ value ^ " </stringConstant>\n")

let tokenize_line line =
  let len = String.length line in

  let rec aux i acc =
    if i >= len then
      List.rev acc
    else
      let c = line.[i] in

            if is_space c then
        aux (i + 1) acc

      else if c = '"' then
        let j = ref (i + 1) in

        while !j < len && line.[!j] <> '"' do
          incr j
        done;

        let str = String.sub line (i + 1) (!j - i - 1) in
        aux (!j + 1) (("\"" ^ str ^ "\"") :: acc)

      else if c = '/' && i + 1 < len && line.[i + 1] = '/' then
        List.rev acc

      else if is_symbol c then
        aux (i + 1) ((String.make 1 c) :: acc)

      else if is_letter c then
        let j = ref i in

        while !j < len && is_identifier_char line.[!j] do
          incr j
        done;

        let token = String.sub line i (!j - i) in
        aux !j (token :: acc)

      else if is_digit c then
        let j = ref i in

        while !j < len && is_digit line.[!j] do
          incr j
        done;

        let token = String.sub line i (!j - i) in
        aux !j (token :: acc)

    
      else
        failwith ("Unknown character: " ^ String.make 1 c)
  in

  aux 0 []

let remove_block_comments line in_comment =
  let len = String.length line in

  let rec aux i acc in_comment =
    if i >= len then
      (String.concat "" (List.rev acc), in_comment)

    else if in_comment then
      (* אנחנו בתוך הערת בלוק, מחפשים את הסיום */ *)
      if i + 1 < len && line.[i] = '*' && line.[i + 1] = '/' then
        aux (i + 2) acc false
      else
        aux (i + 1) acc true

    else
      (* אנחנו לא בתוך הערה *)
      if i + 1 < len && line.[i] = '/' && line.[i + 1] = '*' then
        aux (i + 2) acc true
      else
        aux (i + 1) ((String.make 1 line.[i]) :: acc) false
  in

  aux 0 [] in_comment


let write_tokens ic oc =
  output_string oc "<tokens>\n";

  let rec loop in_comment =
    try
      let line = input_line ic in

      let cleaned_line, new_in_comment =
        remove_block_comments line in_comment
      in

      let tokens = tokenize_line cleaned_line in

      List.iter
        (fun token ->
          if is_string_token token then
            write_string_token oc (remove_quotes token)
          else
            write_token oc token
        )
        tokens;

      loop new_in_comment

    with End_of_file ->
      ()
  in

  loop false;

  output_string oc "</tokens>\n"