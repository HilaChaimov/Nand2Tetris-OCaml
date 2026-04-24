open Parser

let label_counter = ref 0

let fresh_label prefix =
  let n = !label_counter in
  incr label_counter;
  prefix ^ string_of_int n

let emit oc s =
  output_string oc (s ^ "\n")

let current_file = ref ""

let set_file_name file_name =
  current_file := file_name

let push_d_to_stack oc =
  emit oc "@SP";
  emit oc "A=M";
  emit oc "M=D";
  emit oc "@SP";
  emit oc "M=M+1"

let pop_stack_to_d oc =
  emit oc "@SP";
  emit oc "AM=M-1";
  emit oc "D=M"

let write_arithmetic oc cmd =
  match cmd with
  | "add" ->
      emit oc "@SP";
      emit oc "AM=M-1";
      emit oc "D=M";
      emit oc "A=A-1";
      emit oc "M=M+D"

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
      emit oc "D=M-D";
      emit oc ("@" ^ lbl_true);
      emit oc "D;JEQ";
      emit oc "@SP";
      emit oc "A=M-1";
      emit oc "M=0";
      emit oc ("@" ^ lbl_end);
      emit oc "0;JMP";
      emit oc ("(" ^ lbl_true ^ ")");
      emit oc "@SP";
      emit oc "A=M-1";
      emit oc "M=-1";
      emit oc ("(" ^ lbl_end ^ ")")

  | "gt" ->
      let lbl_true = fresh_label "GT_TRUE" in
      let lbl_end = fresh_label "GT_END" in
      emit oc "@SP";
      emit oc "AM=M-1";
      emit oc "D=M";
      emit oc "A=A-1";
      emit oc "D=M-D";
      emit oc ("@" ^ lbl_true);
      emit oc "D;JGT";
      emit oc "@SP";
      emit oc "A=M-1";
      emit oc "M=0";
      emit oc ("@" ^ lbl_end);
      emit oc "0;JMP";
      emit oc ("(" ^ lbl_true ^ ")");
      emit oc "@SP";
      emit oc "A=M-1";
      emit oc "M=-1";
      emit oc ("(" ^ lbl_end ^ ")")

  | "lt" ->
      let lbl_true = fresh_label "LT_TRUE" in
      let lbl_end = fresh_label "LT_END" in
      emit oc "@SP";
      emit oc "AM=M-1";
      emit oc "D=M";
      emit oc "A=A-1";
      emit oc "D=M-D";
      emit oc ("@" ^ lbl_true);
      emit oc "D;JLT";
      emit oc "@SP";
      emit oc "A=M-1";
      emit oc "M=0";
      emit oc ("@" ^ lbl_end);
      emit oc "0;JMP";
      emit oc ("(" ^ lbl_true ^ ")");
      emit oc "@SP";
      emit oc "A=M-1";
      emit oc "M=-1";
      emit oc ("(" ^ lbl_end ^ ")")

  | _ ->
      ()

let write_push_pop oc ct segment index =
  match ct, segment with
  (* push constant i *)
  | CPush, "constant" ->
      emit oc ("@" ^ string_of_int index);
      emit oc "D=A";
      push_d_to_stack oc

  (* push local/argument/this/that i *)
  | CPush, "local" ->
      emit oc "@LCL";
      emit oc "D=M";
      emit oc ("@" ^ string_of_int index);
      emit oc "A=D+A";
      emit oc "D=M";
      push_d_to_stack oc

  | CPush, "argument" ->
      emit oc "@ARG";
      emit oc "D=M";
      emit oc ("@" ^ string_of_int index);
      emit oc "A=D+A";
      emit oc "D=M";
      push_d_to_stack oc

  | CPush, "this" ->
      emit oc "@THIS";
      emit oc "D=M";
      emit oc ("@" ^ string_of_int index);
      emit oc "A=D+A";
      emit oc "D=M";
      push_d_to_stack oc

  | CPush, "that" ->
      emit oc "@THAT";
      emit oc "D=M";
      emit oc ("@" ^ string_of_int index);
      emit oc "A=D+A";
      emit oc "D=M";
      push_d_to_stack oc

  (* pop local/argument/this/that i *)
  | CPop, "local" ->
      emit oc "@LCL";
      emit oc "D=M";
      emit oc ("@" ^ string_of_int index);
      emit oc "D=D+A";
      emit oc "@R13";
      emit oc "M=D";
      pop_stack_to_d oc;
      emit oc "@R13";
      emit oc "A=M";
      emit oc "M=D"

  | CPop, "argument" ->
      emit oc "@ARG";
      emit oc "D=M";
      emit oc ("@" ^ string_of_int index);
      emit oc "D=D+A";
      emit oc "@R13";
      emit oc "M=D";
      pop_stack_to_d oc;
      emit oc "@R13";
      emit oc "A=M";
      emit oc "M=D"

  | CPop, "this" ->
      emit oc "@THIS";
      emit oc "D=M";
      emit oc ("@" ^ string_of_int index);
      emit oc "D=D+A";
      emit oc "@R13";
      emit oc "M=D";
      pop_stack_to_d oc;
      emit oc "@R13";
      emit oc "A=M";
      emit oc "M=D"

  | CPop, "that" ->
      emit oc "@THAT";
      emit oc "D=M";
      emit oc ("@" ^ string_of_int index);
      emit oc "D=D+A";
      emit oc "@R13";
      emit oc "M=D";
      pop_stack_to_d oc;
      emit oc "@R13";
      emit oc "A=M";
      emit oc "M=D"

  (* temp: RAM[5]..RAM[12] *)
  | CPush, "temp" ->
      emit oc ("@" ^ string_of_int (5 + index));
      emit oc "D=M";
      push_d_to_stack oc

  | CPop, "temp" ->
      pop_stack_to_d oc;
      emit oc ("@" ^ string_of_int (5 + index));
      emit oc "M=D"

  (* pointer 0 = THIS, pointer 1 = THAT *)
  | CPush, "pointer" ->
      let ptr =
        if index = 0 then "THIS"
        else if index = 1 then "THAT"
        else failwith "pointer index must be 0 or 1"
      in
      emit oc ("@" ^ ptr);
      emit oc "D=M";
      push_d_to_stack oc

  | CPop, "pointer" ->
      let ptr =
        if index = 0 then "THIS"
        else if index = 1 then "THAT"
        else failwith "pointer index must be 0 or 1"
      in
      pop_stack_to_d oc;
      emit oc ("@" ^ ptr);
      emit oc "M=D"

  (* static *)
  | CPush, "static" ->
      emit oc ("@" ^ !current_file ^ "." ^ string_of_int index);
      emit oc "D=M";
      push_d_to_stack oc

  | CPop, "static" ->
      pop_stack_to_d oc;
      emit oc ("@" ^ !current_file ^ "." ^ string_of_int index);
      emit oc "M=D"

  | _ ->
      failwith "unsupported push/pop command"