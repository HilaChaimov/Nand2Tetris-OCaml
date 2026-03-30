(* hila chaimov 327742185
   talya yakov 327513586 *)

let () =
(*מקבל נתיב תקייה לבדיקה*)
  if Array.length Sys.argv < 2 then
    print_endline "missing directory"
  else
    let path = Sys.argv.(1) in
    
    let files = Array.to_list (Sys.readdir path) in
    let fvm = List.filter (fun f -> Filename.check_suffix f ".vm") files in

    List.iter
      (fun file ->
        (*open input file*)
        let full_path = Filename.concat path file in
        let ic = open_in full_path in
        let base_name = Filename.remove_extension file in
        let fasm = base_name ^ ".asm" in
        let fasm_path = Filename.concat path fasm in
        let oc = open_out fasm_path in
        
        (* לולאת הקריאה והתרגום *)
      let rec loop () =
        match Parser.advance ic with
        | Some words ->
            let ct = Parser.command_type words in
            (match ct with
            | Parser.CArithmetic ->
                let cmd = Parser.arg1 words ct in
                Code_writer.write_arithmetic oc cmd
            | Parser.CPush | Parser.CPop ->
                let segment = Parser.arg1 words ct in
                let index = Parser.arg2 words in
                Code_writer.write_push_pop oc ct segment index
            | _ -> ());
            loop ()
        | None -> ()
      in
      loop ();
      
      close_in ic;
      close_out oc;
    ) fvm