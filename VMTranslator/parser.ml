type command_type =
  | CPush
  | CPop
  | CLabel
  | CArithmetic
  | CGoto
  | CIf
  | CFunction
  | CCall
  | CReturn
  | CUnknown
  

(* פונקציה פנימית לניקוי שורה מהערות ורווחים - עוזרת ל-advance *)
let clean_line line =
  let without_comments = 
    match String.split_on_char '/' line with (* ניקוי הערות *)
    | h :: _ -> String.trim h (* ניקוי רווחים מהתחלה והסוף *)
    | [] -> ""
  in
  without_comments

(* מחזיר רשימת מילים מהשורה אם היא לא ריקה, או None אם הגענו לסוף *)
let rec advance ic =
  try
    let line = input_line ic in
    let cleaned = clean_line line in
    if cleaned = "" then advance ic (* התעלמות משורות ריקות *)
    else Some (String.split_on_char ' ' cleaned |> List.filter (fun s -> s <> ""))
  with End_of_file -> None

(* מחזיר את סוג הפקודה - commandType *)
let command_type words =
  match List.hd words with
  | "push" -> CPush
  | "pop" -> CPop
  | "add" | "sub" | "neg" | "eq" | "gt" | "lt" | "and" | "or" | "not" -> CArithmetic
  | "label" -> CLabel
  | "goto" -> CGoto
  | "if-goto" -> CIf
  | "function" -> CFunction
  | "return" -> CReturn
  | "call" -> CCall
  | _ -> CUnknown


(* מחזיר את הארגומנט הראשון - arg1 *)
let arg1 words ct =
  match ct with
  | CArithmetic ->
     List.hd words (* הפקודות האריתמטיות הן הפקודה עצמה *)
  | CPush | CPop ->
     List.nth words 1 (* הפקודות push ו-pop מכילות את הסגמנט והאינדקס *)
  | CLabel | CGoto | CIf | CFunction | CCall ->
     List.nth words 1 (* פקודות אלו מכילות את שם הלייבל או הפונקציה *)
  | CReturn ->
      failwith "return has no arg1"
  | _ -> ""
    
(* מחזיר את הארגומנט השני - arg2 *)
let arg2 words ct =
  match ct with
  | CPush | CPop | CFunction | CCall ->
      List.nth words 2 |> int_of_string
  | _ ->
      failwith "This command has no arg2"