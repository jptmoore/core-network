open Lwt.Infix;

let monitor = Logs.Src.create("monitor", ~doc="Interface monitor");
module Log = (val (Logs_lwt.src_log(monitor): (module Logs_lwt.LOG)));

let intf_event = l => {
  open Re;
  let up = "(eth[0-9]+).* (([0-9]+.){3}[0-9]+/[0-9]+) .* eth[0-9]+";
  let down = "Deleted.* (([0-9]+.){3}[0-9]+/[0-9]+) ";
  let regex = Printf.sprintf("(%s)|(%s)", up, down);
  try (
    {
      let groups = {
        let re = Re_posix.(regex |> re |> compile);
        exec(re, l) |> Group.all;
      };

      if (groups[2] != "" && groups[3] != "") {
        let link = groups[2]
        and addr = groups[3];
        Some(`Up((link, addr)));
      } else {
        let addr = groups[6];
        Some(`Down(addr));
      };
    }
  ) {
  | Not_found => None
  };
};

let set_link_mtu = (dev, mtu) => {
  let comm = (
    "ip",
    [|"ip", "link", "set", "dev", dev, "mtu", string_of_int(mtu)|],
  );
  Lwt_process.exec(comm)
  >>= (
    fun
    | Unix.WEXITED(0) => Log.info(m => m("set mtu of %s to %d", dev, mtu))
    | _ => Log.warn(m => m("set mtu of %s FAILED, continue", dev))
  );
};

let existed_intf = (interfaces, push_intf) => {
  let command = ("ip", [|"ip", "address", "show"|]);
  let st = Lwt_process.pread_lines(command);
  Lwt_stream.to_list(st)
  >>= (
    lines => {
      let regex = "inet (([0-9]+.){3}[0-9]+/[0-9]+) .*(eth[0-9]+)$";
      let re = Re_posix.(regex |> re |> compile);
      List.fold_left(
        (acc, line) =>
          try (
            {
              let groups = Re.exec(re, line) |> Re.Group.all;
              let dev = groups[3]
              and addr = groups[1];
              [(dev, addr), ...acc];
            }
          ) {
          | Not_found => acc
          },
        [],
        lines,
      )
      |> List.rev
      |> (
        existed =>
          Log.info(m =>
            m("found %d existed phy interfaces", List.length(existed))
          )
          >>= (
            () =>
              Lwt_list.iter_p(
                ((dev, cidr_addr)) =>
                  set_link_mtu(dev, 4000)
                  >>= (
                    () =>
                      Intf.create(~dev, ~cidr=cidr_addr)
                      >>= (
                        ((t, start_t)) => {
                          push_intf(Some(`Up((t, start_t))));
                          Hashtbl.add(interfaces, cidr_addr, dev);
                          Lwt.return_unit;
                        }
                      )
                  ),
                existed,
              )
          )
      );
    }
  );
};

type starter = unit => Lwt.t(unit);

type intf_event = [ | `Up(Intf.t, Intf.starter) | `Down(string)];

let create = () => {
  let interfaces = Hashtbl.create(7);
  let (intf_st, push_intf) = Lwt_stream.create();

  existed_intf(interfaces, push_intf)
  >>= (
    () => {
      let command = ("ip", [|"ip", "monitor", "address"|]);
      let stm = Lwt_process.pread_lines(command);
      let rec monitor_lp = () =>
        Lwt_stream.get(stm)
        >>= (
          fun
          | None =>
            Lwt_io.printf("'ip monitor address' output stream closed!")
          | Some(l) =>
            (
              switch (intf_event(l)) {
              | None => Lwt.return_unit
              | Some(`Up(dev, cidr_addr)) =>
                Log.info(m => m("link up: %s %s", dev, cidr_addr))
                >>= (
                  () =>
                    set_link_mtu(dev, 4000)
                    >>= (
                      () =>
                        Intf.create(~dev, ~cidr=cidr_addr)
                        >>= (
                          ((t, start_t)) => {
                            push_intf(Some(`Up((t, start_t))));
                            Hashtbl.add(interfaces, cidr_addr, dev);
                            Lwt.return_unit;
                          }
                        )
                    )
                )
              | Some(`Down(cidr_addr)) =>
                let dev = Hashtbl.find(interfaces, cidr_addr);
                Log.info(m => m("link down: %s %s", dev, cidr_addr))
                >>= (
                  () => {
                    Hashtbl.remove(interfaces, cidr_addr);
                    push_intf(Some(`Down(dev)));
                    Lwt.return_unit;
                  }
                );
              }
            )
            >>= (() => monitor_lp())
        );

      Lwt.return((intf_st, monitor_lp));
    }
  );
};
