
let t () =
  let ich = open_in "piqi.piqi.pb" in
  let buf = Piqirun.init_from_channel ich in
  let piqi = Piqi.parse_piqi buf in

  let och = open_out "piqi.piqi.pb.pb" in
  let data = Piqi.gen_piqi (-1) piqi in
  Piqirun.to_channel och data;

  close_in ich;
  close_out och;
  ()


let _ = t ()
