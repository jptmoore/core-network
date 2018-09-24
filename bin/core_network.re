open Cmdliner;
open Lib_core_network;

let core = Logs.Src.create("core", ~doc="Databox's core-network component");
module Log = (val (Logs_lwt.src_log(core): (module Logs_lwt.LOG)));

let print_async_exn = () => {
  let hook = Lwt.async_exception_hook^;
  let hook' = exn => {
    Printf.printf(
      "aysnc exception: %s\n%s",
      Printexc.to_string(exn),
      Printexc.get_backtrace(),
    );
    hook(exn);
  };

  Lwt.async_exception_hook := hook';
};

let main = (fifo, logs) =>
  Lwt.Infix.(
    Utils.Log.set_up_logs(logs)
    >>= (
      () =>
        Monitor.create()
        >>= (
          ((intf_st, monitor_starter)) =>
            Log.info(m => m("starting interface monitor..."))
            >>= (
              () => {
                Lwt.async(monitor_starter);
                Lwt_unix.sleep(0.5)
                >>= (
                  () =>
                    Log.info(m => m("starting junction..."))
                    >>= (
                      () =>
                        Junction.create(~fifo?, intf_st)
                        >>= (junction_starter => junction_starter())
                    )
                );
              }
            )
        )
    )
  );

let logs = {
  let doc = "set source-dependent logging level, eg: --logs *:info,foo:debug";
  let src_levels = [
    (`Src("bcast"), Logs.Info),
    (`Src("core"), Logs.Info),
    (`Src("junction"), Logs.Info),
    (`Src("dns"), Logs.Info),
    (`Src("policy"), Logs.Info),
    (`Src("NAT"), Logs.Info),
    (`Src("monitor"), Logs.Info),
    (`Src("interfaces"), Logs.Info),
  ];
  Arg.(
    value
    & opt(list(Utils.Log.log_threshold), src_levels)
    & info(["l", "logs"], ~doc, ~docv="LEVEL")
  );
};

let fifo_name = {
  let doc = "absolute path to fifo to read broadcast packet from";
  Arg.(
    value
    & opt(some(string), None)
    & info(["f", "file"], ~doc, ~docv="FIFO")
  );
};

let cmd = {
  let doc = "databox-bridge core-network";
  (
    Term.(const(main) $ fifo_name $ logs),
    Term.info("bridge", ~doc, ~man=[]),
  );
};

let () = {
  Printexc.record_backtrace(true);
  print_async_exn();
  switch (Term.eval(cmd)) {
  | `Ok(t) => Lwt_main.run(t)
  | _ => exit(1)
  };
};
