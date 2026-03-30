
(* hila chaimov 327742185
   talya yakov 327513586 *)

let () =
  if Array.length Sys.argv < 2 then
    print_endline "missing directory"
  else
    let path = Sys.argv.(1) in
    let dir = Filename.basename path in
    let f = dir ^ ".asm" in
    let f = Filename.concat path f in
    let oc = open_out f in

    (* 1. הגדרת משתנים מצטברים עם ref *)
    let total_buys = ref 0.0 in
    let total_cells = ref 0.0 in

    let handle_buy product_name amount price =
      let total = amount *. price in
      total_buys := !total_buys +. total; (* עדכון המונה המצטבר *)
      let total_str = string_of_float total in
      let text = "### BUY " ^ product_name ^ " ###\n" ^ total_str ^ "\n" in
      output_string oc text
    in

    let handle_cell product_name amount price =
      let total = amount *. price in
      total_cells := !total_cells +. total; (* עדכון המונה המצטבר *)
      let total_str = string_of_float total in
      let text = "$$$ CELL " ^ product_name ^ " $$$\n" ^ total_str ^ "\n" in
      output_string oc text
    in

    let files = Array.to_list (Sys.readdir path) in
    let fvm = List.filter (fun f -> Filename.check_suffix f ".vm") files in

    List.iter
      (fun file ->
        let full_path = Filename.concat path file in
        let ic = open_in full_path in
        let base_name = Filename.remove_extension file in
        output_string oc (base_name ^ "\n");
        try
          while true do
            let line = input_line ic in
            let words = String.split_on_char ' ' line in
            match words with
            | [cmd; a; b; c] ->
                let product_name = a in
                let amount = float_of_string b in
                let price = float_of_string c in
                if cmd = "buy" then
                  handle_buy product_name amount price
                else if cmd = "cell" then
                  handle_cell product_name amount price
                else
                  print_endline "unknown command"
            | _ -> () (* התעלמות משורות ריקות או לא תקינות בלי להציף את המסך *)
          done
        with End_of_file ->
          close_in ic)
      fvm;

    (* 2. הכנת טקסט הסיכום *)
    let summary_text = 
      "Total Buys: " ^ string_of_float !total_buys ^ "\n" ^
      "Total Cells: " ^ string_of_float !total_cells ^ "\n" 
    in

    (* 3. הדפסה למסך *)
    print_string summary_text;

    (* 4. הדפסה לסוף קובץ הפלט *)
    output_string oc summary_text;

    close_out oc
;;