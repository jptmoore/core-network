let secrets_dir = Fpath.v("/run/secrets/");

let cm_key = ref("");

let cm_key = () =>
  if (cm_key^ != "") {
    Rresult.R.ok(cm_key^);
  } else {
    let key_file = Fpath.add_seg(secrets_dir, "DATABOX_NETWORK_KEY");
    let get_key = file => B64.encode(String.trim(file));
    Rresult.R.map(get_key, Bos.OS.File.read(key_file))
    |> (
      fun
      | Ok(key) => {
          cm_key := key;
          Rresult.R.ok(key);
        }
      | Error(msg) => {
          Logs.err(m =>
            m("[env] DATABOX_NETWORK_KEY %a", Rresult.R.pp_msg, msg)
          );
          Error(msg);
        }
    );
  };

let https_creds = () => {
  let cert_file = Fpath.add_seg(secrets_dir, "DATABOX_NETWORK.pem");
  let key_file = Fpath.add_seg(secrets_dir, "DATABOX_NETWORK.pem");
  Rresult.R.ok((cert_file, key_file));
};
