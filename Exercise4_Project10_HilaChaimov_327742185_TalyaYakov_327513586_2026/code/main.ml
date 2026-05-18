let is_jack_file file =
  Filename.check_suffix file ".jack"

let output_path_for_tokenizer jack_path =
  let base = Filename.remove_extension jack_path in
  base ^ "T.xml"

let output_path_for_parser jack_path =
  let base = Filename.remove_extension jack_path in
  base ^ ".xml"

let process_file jack_path =
  (* בשלב הראשון של התרגיל: Tokenizer בלבד *)
  let out_path = output_path_for_tokenizer jack_path in

  let ic = open_in jack_path in
  let oc = open_out out_path in


  Jack_tokenizer.write_tokens ic oc; 

  close_in ic;
  close_out oc

let () =
  if Array.length Sys.argv < 2 then
    print_endline "missing file or directory"
  else
    let path = Sys.argv.(1) in

    if Sys.is_directory path then
      let files = Array.to_list (Sys.readdir path) in
      let jack_files =
        files
        |> List.filter is_jack_file
        |> List.map (fun file -> Filename.concat path file)
      in
      List.iter process_file jack_files
    else
      if is_jack_file path then
        process_file path
      else
        print_endline "input must be a .jack file or a directory"


   