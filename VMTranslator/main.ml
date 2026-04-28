(* hila chaimov 327742185
   talya yakov 327513586 *)


   
let () =
  (* בדיקה שקיבלנו נתיב כארגומנט בהרצה *)
  if Array.length Sys.argv < 2 then
    print_endline "missing directory"
  else
    
    (* הנתיב של התיקייה שהמשתמשת נתנה בהרצה *)
    let path = Sys.argv.(1) in

    (* קריאת כל הקבצים שנמצאים בתיקייה *)
    let files = Array.to_list (Sys.readdir path) in

    (* סינון רק של קבצים שמסתיימים ב־.vm *)
    let fvm = List.filter (fun f -> Filename.check_suffix f ".vm") files in

    (* שינוי לתרגיל 2:
   יוצרים קובץ ASM אחד לכל התיקייה, ולא קובץ ASM נפרד לכל קובץ VM *)
    let dir_name = Filename.basename path in
    let fasm = dir_name ^ ".asm" in
    let fasm_path = Filename.concat path fasm in
    let oc = open_out fasm_path in
    
    List.iter 
      (fun file ->

        (* בניית הנתיב המלא לקובץ ה־VM הנוכחי *)
        let full_path = Filename.concat path file in

        (* פתיחת קובץ ה־VM לקריאה *)
        let ic = open_in full_path in

        (* שמירת שם קובץ ה־VM הנוכחי בשביל static *)
        let base_name = Filename.remove_extension file in
        Code_writer.set_file_name base_name;
        
        (* לולאת הקריאה והתרגום של כל שורה בקובץ VM *)
        let rec loop () =
          match Parser.advance ic with

          (* אם נמצאה פקודת VM תקינה *)
          | Some words ->

              (* זיהוי סוג הפקודה *)
              let ct = Parser.command_type words in

              (* לפי סוג הפקודה מחליטים איך לתרגם אותה *)
              (match ct with

              (* פקודה אריתמטית כמו add / sub / eq וכו' *)
              | Parser.CArithmetic ->
                  let cmd = Parser.arg1 words ct in
                  Code_writer.write_arithmetic oc cmd

              (* פקודת push או pop *)
              | Parser.CPush | Parser.CPop ->
                  let segment = Parser.arg1 words ct in
                  let index = Parser.arg2 words ct in
                  Code_writer.write_push_pop oc ct segment index
             
              (* פקודות לוגיות *)
              | Parser.CLabel |Parser.CGoto | Parser.CIf ->
                  let label = Parser.arg1 words ct in
                  Code_writer.write_label_goto_if oc ct label
              
              (* פקודות פונקציה *)
              | Parser.CFunction | Parser.CCall ->
                  let func_name = Parser.arg1 words ct in
                  let n_args = Parser.arg2 words ct in
                  Code_writer.write_function_call oc ct func_name n_args

              | Parser.CReturn ->
                  Code_writer.write_return oc
            
              | _ -> ());

              (* ממשיכים לשורה הבאה *)
              loop ()

          (* סוף הקובץ *)
          | None -> ()
        in

        (* הפעלת לולאת התרגום *)
        loop ();
      
        (* סגירת קובץ הקלט *)
        close_in ic;

      ) fvm;

    (* סגירת קובץ הפלט *)
    close_out oc