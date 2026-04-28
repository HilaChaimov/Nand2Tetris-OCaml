open Parser

(*פעולה בשביל יצירת תווים ייחודיים לתוויות*)
let label_counter = ref 0

(* פונקציה ליצירת תווית ייחודית עם פריפיקס נתון *)
let fresh_label prefix =
  let n = !label_counter in (*הסימן קריאה- להוציא את הערך מתןך label_counter*)
  incr label_counter; (*label_counter := n + 1*)
  prefix ^ string_of_int n

(* פונקציה עזר להדפסת פקודות ASM *)
let emit oc s =
  output_string oc (s ^ "\n")

(* שמירת שם הקובץ הנוכחי כדי להשתמש בו בפקודות static *)
let current_file = ref ""

(* פונקציה להגדרת שם הקובץ הנוכחי *)
let set_file_name file_name =
  current_file := file_name

(* פונקציית עזר לכתיבת פקודות push *)
let push_d_to_stack oc =
  emit oc "@SP";
  emit oc "A=M";
  emit oc "M=D";
  emit oc "@SP";
  emit oc "M=M+1"

(* פונקציית עזר לכתיבת פקודות pop *)
let pop_stack_to_d oc =
  emit oc "@SP";
  emit oc "AM=M-1";
  emit oc "D=M"

let write_arithmetic oc cmd =

  match cmd with
  | "add" ->
      emit oc "@SP";
      emit oc "AM=M-1"; (*הורדת הכתובת של האיבר העליון מהסטאק והכנסתו ל-A*)
      emit oc "D=M"; (*הכנסת הערך של האיבר העליון ל-D*)
      emit oc "A=A-1";
      emit oc "M=M+D"(*הוספת הערך של האיבר העליון לערך של האיבר השני מהסטאק ושמירת התוצאה באיבר השני*)

  | "sub" ->
      emit oc "@SP";
      emit oc "AM=M-1";
      emit oc "D=M";
      emit oc "A=A-1";
      emit oc "M=M-D" 

  | "neg" ->
      emit oc "@SP";
      emit oc "A=M-1";
      emit oc "M=-M"

  | "and" ->
      emit oc "@SP";
      emit oc "AM=M-1";
      emit oc "D=M";
      emit oc "A=A-1";
      emit oc "M=M&D"

  | "or" ->
      emit oc "@SP";
      emit oc "AM=M-1";
      emit oc "D=M";
      emit oc "A=A-1";
      emit oc "M=M|D"

  | "not" ->
      emit oc "@SP";
      emit oc "A=M-1";
      emit oc "M=!M"

  | "eq" ->
    let lbl_true = fresh_label "EQ_TRUE" in
    let lbl_end = fresh_label "EQ_END" in
    emit oc "@SP";
    emit oc "AM=M-1";
    emit oc "D=M";
    emit oc "A=A-1";
    emit oc "D=M-D"; (* בדיקה האם האיבר השני מהסטאק שווה לאיבר העליון *)
    emit oc ("@" ^ lbl_true); (* הכנת קפיצה לתווית true *)
    emit oc "D;JEQ"; (* אם הם שווים, קפוץ לתווית lbl_true *)
    emit oc "@SP"; (* אם לא, המשך לכתוב 0 לסטאק *)
    emit oc "A=M-1"; (* הכנס את הכתובת של האיבר השני מהסטאק ל-A *)
    emit oc "M=0"; (* כתוב 0 באיבר השני מהסטאק — false *)
    emit oc ("@" ^ lbl_end); (* הכנת קפיצה לתווית הסיום *)
    emit oc "0;JMP"; (* קפוץ לתווית הסיום כדי לא להיכנס לקטע true *)
    emit oc ("(" ^ lbl_true ^ ")"); (* אם הגענו לכאן, ההשוואה true *)
    emit oc "@SP"; (* חזרה ל-SP *)
    emit oc "A=M-1"; (* הכנס את הכתובת של האיבר השני מהסטאק ל-A *)
    emit oc "M=-1"; (* כתוב -1 באיבר השני מהסטאק — true *)
    emit oc ("(" ^ lbl_end ^ ")") (* תווית סיום ההשוואה *)

  | "gt" ->
    let lbl_true = fresh_label "GT_TRUE" in
    let lbl_end = fresh_label "GT_END" in
    emit oc "@SP";
    emit oc "AM=M-1";
    emit oc "D=M";
    emit oc "A=A-1";
    emit oc "D=M-D"; (* בדיקה האם האיבר השני מהסטאק גדול מהאיבר העליון *)
    emit oc ("@" ^ lbl_true); (* הכנת קפיצה לתווית true *)
    emit oc "D;JGT"; (* אם הוא גדול, קפוץ לתווית lbl_true *)
    emit oc "@SP"; (* אם לא, המשך לכתוב 0 לסטאק *)
    emit oc "A=M-1"; (* הכנס את הכתובת של האיבר השני מהסטאק ל-A *)
    emit oc "M=0"; (* כתוב 0 באיבר השני מהסטאק — false *)
    emit oc ("@" ^ lbl_end); (* הכנת קפיצה לתווית הסיום *)
    emit oc "0;JMP"; (* קפוץ לתווית הסיום כדי לא להיכנס לקטע true *)
    emit oc ("(" ^ lbl_true ^ ")"); (* אם הגענו לכאן, ההשוואה true *)
    emit oc "@SP"; (* חזרה ל-SP *)
    emit oc "A=M-1"; (* הכנס את הכתובת של האיבר השני מהסטאק ל-A *)
    emit oc "M=-1"; (* כתוב -1 באיבר השני מהסטאק — true *)
    emit oc ("(" ^ lbl_end ^ ")") (* תווית סיום ההשוואה *)

  | "lt" ->
    let lbl_true = fresh_label "LT_TRUE" in
    let lbl_end = fresh_label "LT_END" in
    emit oc "@SP";
    emit oc "AM=M-1";
    emit oc "D=M";
    emit oc "A=A-1";
    emit oc "D=M-D"; (* בדיקה האם האיבר השני מהסטאק קטן מהאיבר העליון *)
    emit oc ("@" ^ lbl_true); (* הכנת קפיצה לתווית true *)
    emit oc "D;JLT"; (* אם הוא קטן, קפוץ לתווית lbl_true *)
    emit oc "@SP"; (* אם לא, המשך לכתוב 0 לסטאק *)
    emit oc "A=M-1"; (* הכנס את הכתובת של האיבר השני מהסטאק ל-A *)
    emit oc "M=0"; (* כתוב 0 באיבר השני מהסטאק — false *)
    emit oc ("@" ^ lbl_end); (* הכנת קפיצה לתווית הסיום *)
    emit oc "0;JMP"; (* קפוץ לתווית הסיום כדי לא להיכנס לקטע true *)
    emit oc ("(" ^ lbl_true ^ ")"); (* אם הגענו לכאן, ההשוואה true *)
    emit oc "@SP"; (* חזרה ל-SP *)
    emit oc "A=M-1"; (* הכנס את הכתובת של האיבר השני מהסטאק ל-A *)
    emit oc "M=-1"; (* כתוב -1 באיבר השני מהסטאק — true *)
    emit oc ("(" ^ lbl_end ^ ")") (* תווית סיום ההשוואה *)
  | _ ->
      ()

let write_push_pop oc ct segment index =
  match ct, segment with

  (* push constant i: דוחפים לסטאק את המספר עצמו, לא ערך מהזיכרון *)
  | CPush, "constant" ->
      emit oc ("@" ^ string_of_int index);
      emit oc "D=A"; (* D = index *)
      push_d_to_stack oc

  (* push local/argument/this/that i: הסגמנטים האלה עובדים לפי כתובת בסיס + אינדקס *)
  | CPush, "local" ->
      emit oc "@LCL";
      emit oc "D=M"; (* D = כתובת הבסיס של local *)
      emit oc ("@" ^ string_of_int index); 
      emit oc "A=D+A"; (* A = LCL + index *)
      emit oc "D=M"; (* D = הערך שנמצא ב־local[index] *)
      push_d_to_stack oc

  | CPush, "argument" ->
      emit oc "@ARG";
      emit oc "D=M"; (* D = כתובת הבסיס של argument *)
      emit oc ("@" ^ string_of_int index);
      emit oc "A=D+A"; (* A = ARG + index *)
      emit oc "D=M";
      push_d_to_stack oc

  | CPush, "this" ->
      emit oc "@THIS";
      emit oc "D=M"; (* D = כתובת הבסיס של this *)
      emit oc ("@" ^ string_of_int index);
      emit oc "A=D+A"; (* A = THIS + index *)
      emit oc "D=M";
      push_d_to_stack oc

  | CPush, "that" ->
      emit oc "@THAT";
      emit oc "D=M"; (* D = כתובת הבסיס של that *)
      emit oc ("@" ^ string_of_int index);
      emit oc "A=D+A"; (* A = THAT + index *)
      emit oc "D=M";
      push_d_to_stack oc

  (* pop local/argument/this/that i:
 קודם מחשבים את כתובת היעד ושומרים ב־R13,
     כי פעולת pop משתמשת ב־D ותדרוס לנו את הכתובת *)
  | CPop, "local" ->
      emit oc "@LCL";
      emit oc "D=M"; (* D = כתובת הבסיס של local *)
      emit oc ("@" ^ string_of_int index);
      emit oc "D=D+A"; (* D = LCL + index *)
      emit oc "@R13";
      emit oc "M=D"; (* R13 = כתובת היעד *)
      pop_stack_to_d oc; (* D = הערך העליון מהסטאק *)
      emit oc "@R13";
      emit oc "A=M"; (* A = כתובת היעד *)
      emit oc "M=D" (* כתיבת הערך ל־local[index] *)

  | CPop, "argument" ->
      emit oc "@ARG";
      emit oc "D=M"; (* D = כתובת הבסיס של argument *)
      emit oc ("@" ^ string_of_int index);
      emit oc "D=D+A"; (* D = ARG + index *)
      emit oc "@R13";
      emit oc "M=D";
      pop_stack_to_d oc;
      emit oc "@R13";
      emit oc "A=M";
      emit oc "M=D"

  | CPop, "this" ->
      emit oc "@THIS";
      emit oc "D=M"; (* D = כתובת הבסיס של this *)
      emit oc ("@" ^ string_of_int index);
      emit oc "D=D+A"; (* D = THIS + index *)
      emit oc "@R13";
      emit oc "M=D";
      pop_stack_to_d oc;
      emit oc "@R13";
      emit oc "A=M";
      emit oc "M=D"

  | CPop, "that" ->
      emit oc "@THAT";
      emit oc "D=M"; (* D = כתובת הבסיס של that *)
      emit oc ("@" ^ string_of_int index);
      emit oc "D=D+A"; (* D = THAT + index *)
      emit oc "@R13";
      emit oc "M=D";
      pop_stack_to_d oc;
      emit oc "@R13";
      emit oc "A=M";
      emit oc "M=D"

  (* temp:
     סגמנט קבוע בזיכרון: temp 0 = RAM[5], ..., temp 7 = RAM[12] *)
  | CPush, "temp" ->
      emit oc ("@" ^ string_of_int (5 + index)); (* A = 5 + index *)
      emit oc "D=M"; (* D = temp[index] *)
      push_d_to_stack oc

  | CPop, "temp" ->
      pop_stack_to_d oc; (* D = הערך העליון מהסטאק *)
      emit oc ("@" ^ string_of_int (5 + index)); (* A = 5 + index *)
      emit oc "M=D" (* temp[index] = D *)

  (* pointer: pointer 0 הוא THIS, pointer 1 הוא THAT *)
  | CPush, "pointer" ->
      let ptr =
        if index = 0 then "THIS"
        else if index = 1 then "THAT"
        else failwith "pointer index must be 0 or 1"
      in
      emit oc ("@" ^ ptr); (* בחירה בין THIS ל־THAT *)
      emit oc "D=M";
      push_d_to_stack oc

  | CPop, "pointer" ->
      let ptr =
        if index = 0 then "THIS"
        else if index = 1 then "THAT"
        else failwith "pointer index must be 0 or 1"
      in
      pop_stack_to_d oc; (* D = הערך העליון מהסטאק *)
      emit oc ("@" ^ ptr); (* בחירה בין THIS ל־THAT *)
      emit oc "M=D" (* THIS/THAT = D *)

  (* static: משתנה סטטי מקבל שם לפי הקובץ הנוכחי, למשל Class1.0 *)
  (*static =משתנה בזיכרון עם שם לפי הקובץ כדי למנוע התנגשויות בין קבצים*)
  | CPush, "static" ->
      emit oc ("@" ^ !current_file ^ "." ^ string_of_int index); (* A = current_file.index *)
      emit oc "D=M";
      push_d_to_stack oc

  | CPop, "static" ->
      pop_stack_to_d oc; (* D = הערך העליון מהסטאק *)
      emit oc ("@" ^ !current_file ^ "." ^ string_of_int index); (* A = current_file.index *)
      emit oc "M=D"

  | _ ->
      failwith "unsupported push/pop command"

let write_label_goto_if oc ct label =
  match ct with
  | CLabel ->
      emit oc ("(" ^ label ^ ")") (* הגדרת תווית *)
  | CGoto ->
      emit oc ("@" ^ label); (* הכנת קפיצה לתווית *)
      emit oc "0;JMP" (* קפיצה בלתי מותנית *)
  | CIf ->
      pop_stack_to_d oc; (* D = הערך העליון מהסטאק *)
      emit oc ("@" ^ label); (* הכנת קפיצה לתווית *)
      emit oc "D;JNE" (* אם D לא שווה ל־0, קפוץ לתווית *)
  | _ ->
      failwith "unsupported label/goto/if command"

let write_function_call oc ct func_name n_args =
  failwith "function/call not implemented yet"

let write_return oc =
  failwith "return not implemented yet"