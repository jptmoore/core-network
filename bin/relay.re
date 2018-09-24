open Lwt.Infix;
open Lib_core_network;

let intf = Logs.Src.create("relay", ~doc="Bcast traffic relay");
module Log = (val (Logs_lwt.src_log(intf): (module Logs_lwt.LOG)));

let existed_intf = () => {
  let command = ("ip", [|"ip", "address", "show"|]);
  let st = Lwt_process.pread_lines(command);
  Lwt_stream.to_list(st)
  >>= (
    lines => {
      let regex = "inet (([0-9]+.){3}[0-9]+/[0-9]+) .* ([a-zA-Z0-9_]+)$";
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
                  Log.debug(m => m("%s: %s", dev, cidr_addr)),
                existed,
              )
              >>= (() => Lwt.return(existed))
          )
      );
    }
  );
};

let host_net = (host_ip, existed) =>
  List.fold_left(
    (acc, (dev, cidr_addr)) => {
      let (_, ip) = Ipaddr.V4.Prefix.of_address_string_exn(cidr_addr);
      if (host_ip == Ipaddr.V4.to_string(ip)) {
        [(dev, cidr_addr), ...acc];
      } else {
        acc;
      };
    },
    [],
    existed,
  )
  |> (
    hosts =>
      if (0 != List.length(hosts)) {
        let (dev, cidr) = List.hd(hosts);
        Log.info(m => m("Creat Intf with %s %s", dev, cidr))
        >>= (() => Intf.create(~dev, ~cidr) >>= Lwt.return_some);
      } else {
        Log.warn(m => m("no interface with address %s found", host_ip))
        >>= (
          () =>
            Log.warn(m => m("broadcast relay not supported"))
            >>= (() => Lwt.return_none)
        );
      }
  );

let broadcast = (consume, intf) => {
  let broad_dst = Ipaddr.V4.Prefix.broadcast(intf.Intf.network);
  let recvfrom = (intf, buf) =>
    Lwt_stream.get(intf.Intf.recv_st)
    >>= (
      fun
      | Some(pkt) => {
          let dst = Ipv4_wire.get_ipv4_dst(pkt) |> Ipaddr.V4.of_int32;
          if (0 == Ipaddr.V4.compare(dst, broad_dst)) {
            let pkt_len = Ipv4_wire.get_ipv4_len(pkt);
            Log.debug(m => m("got one broadcast pkt: %d", pkt_len))
            >>= (() => consume(pkt, buf));
          } else {
            Lwt.return_unit;
          };
        }
      | None => Log.warn(m => m("recv stream from %s closed!", intf.Intf.dev))
    );
  let rec loop = (buf, ()) => recvfrom(intf, buf) >>= loop(buf);
  let buf = Cstruct.create(4096);
  loop(buf, ());
};

/* name: absolute path */
let open_fifo = name =>
  /*check for existence and file type*/
  Lwt_unix.openfile(name, [Unix.O_WRONLY], 0o640);

let write_fifo = (fd, pkt, buf) => {
  let len = Cstruct.len(pkt);
  let () = Cstruct.BE.set_uint16(buf, 0, len);
  let () = Cstruct.blit(pkt, 0, buf, 2, len);
  let wbuf = Cstruct.set_len(buf, 2 + len);
  let rec write = chk => {
    let clen = Cstruct.len(chk);
    Lwt_cstruct.write(fd, chk)
    >>= (
      wlen =>
        if (clen == wlen) {
          Lwt.return_unit;
        } else {
          write(Cstruct.shift(chk, wlen));
        }
    );
  };
  write(wbuf);
};

open Cmdliner;

let main = (host_ip, fifo, logs) =>
  Utils.Log.set_up_logs(logs)
  >>= (
    () =>
      open_fifo(fifo)
      >>= (
        fd =>
          Log.info(m => m("Opened %s for write bcast pkts.", fifo))
          >>= (
            () =>
              existed_intf()
              >>= host_net(host_ip)
              >>= (
                fun
                | Some((intf, intf_starter)) =>
                  broadcast(write_fifo(fd), intf) <&> intf_starter()
                | None => {
                    let (t, _) = Lwt.wait();
                    t;
                  }
              )
          )
      )
  );

let logs = {
  let doc = "set source-dependent logging level, eg: --logs *:info,foo:debug";
  let src_levels = [(`Src("relay"), Logs.Debug)];
  Arg.(
    value
    & opt(list(Utils.Log.log_threshold), src_levels)
    & info(["l", "logs"], ~doc, ~docv="LEVEL")
  );
};

let host = {
  let doc = "set host IP address of relay broadcast traffic from";
  Arg.(
    required
    & opt(some(string), None)
    & info(["h", "host"], ~doc, ~docv="HOST")
  );
};

let fifo_name = {
  let doc = "absolute path to fifo to write broadcast packet into";
  Arg.(
    required
    & opt(some(string), None)
    & info(["f", "file"], ~doc, ~docv="FIFO")
  );
};

let cmd = {
  let doc = "broadcast traffic relay from host to core-network";
  (
    Term.(const(main) $ host $ fifo_name $ logs),
    Term.info("relay", ~doc, ~man=[]),
  );
};

let () =
  switch (Term.eval(cmd)) {
  | `Ok(t) => Lwt_main.run(t)
  | _ => exit(1)
  };
