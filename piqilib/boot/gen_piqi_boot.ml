
let load_file fname =
  let ch = open_in fname in
  let len = in_channel_length ch in
  let res = String.create len in
  really_input ch res 0 len;
  close_in ch;
  res


let main () =
  let piqi_lang_binobj = load_file "piqi-lang.piqi.pb" in
  let piqi_spec_binobj = load_file "piqi.piqi.pb" in

  Printf.printf "let parse_piqi_binobj x = Piqirun.parse_binobj Piqi_impl_piqi.parse_piqi x\n\n";

  Printf.printf "let piqi_lang =\n";
  Printf.printf "  let piqi_lang_binobj =\n";
  Printf.printf "    \"%s\"\n" (String.escaped piqi_lang_binobj);
  Printf.printf "  in parse_piqi_binobj piqi_lang_binobj\n\n";

  Printf.printf "let piqi_spec =\n";
  Printf.printf "  let piqi_spec_binobj =\n";
  Printf.printf "    \"%s\"\n" (String.escaped piqi_spec_binobj);
  Printf.printf "  in parse_piqi_binobj piqi_spec_binobj\n\n";
  ()


let _ =
  main ()

