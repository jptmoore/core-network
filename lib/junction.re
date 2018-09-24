open Lwt.Infix;

let junction = Logs.Src.create("junction", ~doc="Traffic junction");
module Log = (val (Logs_lwt.src_log(junction): (module Logs_lwt.LOG)));

let hexdump_buf_debug = (desp, buf) =>
  Log.warn(m => {
    let b = Buffer.create(4096);
    Cstruct.hexdump_to_buffer(b, buf);
    m("%s len:%d pkt:%s", desp, Cstruct.len(buf), Buffer.contents(b));
  });

let pp_ip = Ipaddr.V4.pp_hum;

module Local = {
  module Backend = Basic_backend.Make;
  module Service = Server.Make(Backend);

  type t = {
    id: int,
    backend: Backend.t,
    service: Service.t,
    address: Ipaddr.V4.t,
    network: Ipaddr.V4.Prefix.t,
  };

  let instance = ref(None);

  let is_to_local = ip =>
    switch (instance^) {
    | None => false
    | Some({address}) => 0 == Ipaddr.V4.compare(ip, address)
    };

  let local_virtual_mac = Macaddr.make_local(x => x + 1);
  let write_to_service = (ethertype, pkt) =>
    switch (instance^) {
    | None => Log.err(m => m("Local service not initialized!"))
    | Some(t) =>
      open Ethif_packet;
      let source = local_virtual_mac;
      let destination = Service.mac(t.service);
      let hd = {source, destination, ethertype};
      let hd = Marshal.make_cstruct(hd);
      let fr = Cstruct.append(hd, pkt);
      Backend.write(t.backend, t.id, fr)
      >>= (
        fun
        | Ok () => Lwt.return_unit
        | Error(err) =>
          Log.err(m =>
            m("write_to_service err: %a", Mirage_net.pp_error, err)
          )
          >>= Lwt.return
      );
    };

  /*from local stack in Server*/
  let set_local_listener = (t, intf) => {
    open Frame;
    let listener = buf =>
      Lwt.catch(
        () =>
          switch (parse(buf)) {
          | Ok(Ethernet({dst: dst_mac, payload: Ipv4({dst: dst_ip})}))
              when 0 == Macaddr.compare(dst_mac, local_virtual_mac) =>
            let pkt_raw = Cstruct.shift(buf, Ethif_wire.sizeof_ethernet);
            intf.Intf.send_push @@ Some(pkt_raw);
            Lwt.return_unit;
          | Ok(
              Ethernet({
                src: tha,
                payload: Arp({op: `Request, spa: tpa, tpa: spa}),
              }),
            ) =>
            let arp_resp = {
              open Arpv4_packet;
              let t = {
                op: Arpv4_wire.Reply,
                sha: local_virtual_mac,
                spa,
                tha,
                tpa,
              };
              Marshal.make_cstruct(t);
            };

            write_to_service(Ethif_wire.ARP, arp_resp);
          | Ok(fr) =>
            Log.warn(m =>
              m("not ipv4 or arp request: %s, dropped", fr_info(fr))
            )
          | Error(`Msg(msg)) =>
            Log.err(m => m("parse pkt from local err: %s", msg))
          },
        e =>
          Log.err(m =>
            m(
              "set_local_listener: %s\n%s",
              Printexc.to_string(e),
              Printexc.get_backtrace(),
            )
          )
          >>= (() => Lwt.fail(e)),
      );

    Backend.set_listen_fn(t.backend, t.id, listener);
  };

  let create = intf => {
    let yield = Lwt_main.yield;
    let use_async_readers = true;
    let backend = Backend.create(~yield, ~use_async_readers, ());
    let id =
      switch (Backend.register(backend)) {
      | `Ok(id) => id
      | `Error(err) =>
        Log.err(m => m("Backend.register err: %a", Mirage_net.pp_error, err))
        |> Lwt.ignore_result;
        (-1);
      };

    let address = intf.Intf.ip;
    let network = intf.Intf.network;
    Service.make(backend, address)
    >>= (
      service => {
        let t = {id, backend, service, network, address};
        set_local_listener(t, intf);
        instance := Some(t);
        Lwt.return(t);
      }
    );
  };

  open Service;

  let connect_for = po => {
    let connect_handler = req =>
      Lwt.catch(
        () =>
          json_of_body_exn(req)
          >>= (
            obj =>
              try (
                {
                  open Ezjsonm;
                  let dict = get_dict(value(obj));
                  let name = List.assoc("name", dict) |> get_string;
                  let peers =
                    List.assoc("peers", dict) |> get_list(get_string);
                  Lwt_list.map_p(
                    peer => Policy.connect(po, name, peer),
                    peers,
                  )
                  >>= (
                    _ => {
                      let status = Cohttp.Code.(`OK);
                      Lwt.return((status, `Json(`O([]))));
                    }
                  );
                }
              ) {
              | e => Lwt.fail(e)
              }
          ),
        e => {
          let msg =
            Printf.sprintf("/connect server err: %s", Printexc.to_string(e));
          let status = Cohttp.Code.(`Code(500));
          Lwt.return((status, `String(msg)));
        },
      )
      >>= (((code, body)) => respond'(~code, body));

    post("/connect", connect_handler);
  };

  let disconnect_for = po => {
    let disconnect_handler = req =>
      Lwt.catch(
        () =>
          json_of_body_exn(req)
          >>= (
            obj =>
              try (
                {
                  open Ezjsonm;
                  let dict = get_dict(value(obj));
                  let name = List.assoc("name", dict) |> get_string;
                  let ip =
                    List.assoc("ip", dict)
                    |> get_string
                    |> Ipaddr.V4.Prefix.of_address_string_exn
                    |> snd;
                  Policy.disconnect(po, name, ip)
                  >>= (
                    () => {
                      let status = Cohttp.Code.(`OK);
                      Lwt.return((status, `Json(`O([]))));
                    }
                  );
                }
              ) {
              | e => Lwt.fail(e)
              }
          ),
        e => {
          let msg =
            Printf.sprintf(
              "/disconnect server err: %s",
              Printexc.to_string(e),
            );
          let status = Cohttp.Code.(`Code(500));
          Lwt.return((status, `String(msg)));
        },
      )
      >>= (((code, body)) => respond'(~code, body));

    post("/disconnect", disconnect_handler);
  };

  let service_restart = po => {
    let service_request_handler = req =>
      Lwt.catch(
        () =>
          json_of_body_exn(req)
          >>= (
            obj =>
              try (
                {
                  open Ezjsonm;
                  let dict = get_dict(value(obj));
                  let name = List.assoc("name", dict) |> get_string;
                  let old_ip =
                    List.assoc("old_ip", dict)
                    |> get_string
                    |> Ipaddr.V4.of_string_exn;
                  let new_ip =
                    List.assoc("new_ip", dict)
                    |> get_string
                    |> Ipaddr.V4.of_string_exn;
                  Policy.substitute(po, name, old_ip, new_ip)
                  >>= (
                    () => {
                      let status = Cohttp.Code.(`OK);
                      Lwt.return((status, `Json(`O([]))));
                    }
                  );
                }
              ) {
              | e => Lwt.fail(e)
              }
          ),
        e => {
          let msg =
            Printf.sprintf("/restart server err: %s", Printexc.to_string(e));
          let status = Cohttp.Code.(`Code(500));
          Lwt.return((status, `String(msg)));
        },
      )
      >>= (((code, body)) => respond'(~code, body));

    post("/restart", service_request_handler);
  };

  let add_privileged = (po, network) => {
    let add_privileged_handler = req =>
      Lwt.catch(
        () =>
          json_of_body_exn(req)
          >>= (
            obj =>
              try (
                {
                  open Ezjsonm;
                  let dict = get_dict(value(obj));
                  let src_ip_str = List.assoc("src_ip", dict) |> get_string;
                  let src_ip = Ipaddr.V4.of_string_exn(src_ip_str);
                  Policy.allow_privileged_ip(po, src_ip)
                  >>= (
                    () =>
                      Policy.disallow_privileged_network(po, network)
                      >>= (() => Lwt.return((`OK, `Json(`O([])))))
                  );
                }
              ) {
              | e => Lwt.fail(e)
              }
          ),
        e => {
          let msg =
            Printf.sprintf(
              "/privileged server err: %s",
              Printexc.to_string(e),
            );
          Lwt.return((`Code(500), `String(msg)));
        },
      )
      >>= (((code, body)) => respond'(~code, body));

    post("/privileged", add_privileged_handler);
  };

  let get_status = {
    let status_handler = req => respond'(~code=`OK, `String("active"));
    get("/status", status_handler);
  };

  let start_service = (t, po) => {
    let callback =
      callback_of_routes([
        connect_for(po),
        disconnect_for(po),
        service_restart(po),
        add_privileged(po, t.network),
        get_status,
      ]);
    start(t.service, ~callback);
  };

  let initialize = (intf, policy) =>
    create(intf) >>= (t => Lwt.return @@ start_service(t, policy));
};

module Dispatcher = {
  type t = {
    interfaces: Interfaces.t,
    policy: Policy.t,
    nat: Nat.t,
  };

  let dispatch = (t, (buf, pkt)) => {
    let (src_ip, dst_ip, ihl) =
      Frame.(
        switch (pkt) {
        | Ipv4({src, dst, ihl, _}) => (src, dst, ihl)
        | _ =>
          Log.err(m => m("Dispathcer: dispatch %s", Frame.fr_info(pkt)))
          |> Lwt.ignore_result;
          assert(false);
        }
      );
    if (Dns_service.is_dns_query(pkt)) {
      Log.debug(m => m("Dispatcher: a dns query from %a", pp_ip, src_ip))
      >>= (
        () => {
          let resolve = Policy.is_authorized_resolve(t.policy, src_ip);
          Dns_service.process_dns_query(~resolve, pkt)
          >>= (
            resp => {
              let resp = Dns_service.to_dns_response(pkt, resp);
              Interfaces.to_push(t.interfaces, dst_ip, fst(resp));
            }
          );
        }
      );
    } else if (Local.is_to_local(dst_ip)) {
      Local.write_to_service(Ethif_wire.IPv4, buf);
    } else if (Policy.is_authorized_transport(t.policy, src_ip, dst_ip)) {
      Nat.translate(t.nat, (src_ip, dst_ip), (buf, pkt))
      >>= (
        ((nat_src_ip, nat_dst_ip, nat_buf, nat_pkt)) =>
          Log.debug(m =>
            m(
              "Dispatcher: allowed pkt[NAT] %a -> %a => %a -> %a",
              pp_ip,
              src_ip,
              pp_ip,
              dst_ip,
              pp_ip,
              nat_src_ip,
              pp_ip,
              nat_dst_ip,
            )
          )
          >>= (() => Interfaces.to_push(t.interfaces, nat_src_ip, nat_buf))
      );
    } else if (Dns_service.is_dns_response(pkt)) {
      Lwt.return_unit;
    } else {
      Log.warn(m =>
        m("Dispatcher: dropped pkt %a -> %a", pp_ip, src_ip, pp_ip, dst_ip)
      );
    };
  };

  let create = (interfaces, nat, policy) => {interfaces, policy, nat};
};

let create = (~fifo=?, intf_st) => {
  let interfaces = Interfaces.create();
  let nat = Nat.create();
  let policy = Policy.create(interfaces, nat);
  let dispatcher = Dispatcher.create(interfaces, nat, policy);
  let dispatch_fn = Dispatcher.dispatch(dispatcher);

  let register_and_start = (intf, intf_starter) =>
    Interfaces.register_intf(interfaces, intf, dispatch_fn)
    >>= (
      interfaces_starter => {
        let t = () =>
          Lwt.finalize(
            () =>
              Lwt.catch(
                () =>
                  Log.info(m =>
                    m(
                      "register intf %s %a %a",
                      intf.Intf.dev,
                      pp_ip,
                      intf.Intf.ip,
                      Ipaddr.V4.Prefix.pp_hum,
                      intf.Intf.network,
                    )
                  )
                  >>= (() => Lwt.join([intf_starter(), interfaces_starter()])),
                exn =>
                  Log.err(m =>
                    m(
                      "intf %s err: %s",
                      intf.Intf.dev,
                      Printexc.to_string(exn),
                    )
                  ),
              ),
            () => Log.info(m => m("intf %s exited!", intf.Intf.dev)),
          );
        Lwt.return @@ Lwt.async(t);
      }
    );

  let rec junction_lp = () =>
    Lwt_stream.get(intf_st)
    >>= (
      fun
      | None => Log.warn(m => m("monitor stream closed!"))
      | Some(`Up(intf, intf_starter)) =>
        if (intf.Intf.dev == "eth0") {
          Policy.allow_privileged_network(policy, intf.Intf.network)
          >>= (
            () =>
              Local.initialize(intf, policy)
              >>= (
                service_starter =>
                  register_and_start(intf, intf_starter)
                  >>= (
                    () =>
                      Log.info(m =>
                        m("start local service on %s...", intf.Intf.dev)
                      )
                      >>= (
                        () => {
                          Lwt.async(service_starter);
                          junction_lp();
                        }
                      )
                  )
              )
          );
        } else if (intf.dev == "eth1") {
          let network = intf.Intf.network;
          let gw =
            network
            |> Ipaddr.V4.Prefix.network
            |> Ipaddr.V4.to_int32
            |> Int32.add(Int32.one)
            |> Ipaddr.V4.of_int32;
          Intf.set_gateway(intf, gw);
          Log.info(m =>
            m(
              "set gateway for %s(%a) to %a",
              intf.Intf.dev,
              Ipaddr.V4.Prefix.pp_hum,
              intf.Intf.network,
              Ipaddr.V4.pp_hum,
              gw,
            )
          )
          >>= (
            () =>
              register_and_start(intf, intf_starter) >>= (() => junction_lp())
          );
        } else {
          register_and_start(intf, intf_starter) >>= (() => junction_lp());
        }
      | Some(`Down(dev)) =>
        Interfaces.deregister_intf(interfaces, dev) >>= (() => junction_lp())
    );

  Policy.allow_privileged_host(policy, "arbiter")
  >>= (
    () =>
      Bcast.create(~fifo?, interfaces)
      >>= (
        bcast_starter =>
          Lwt.return @@ (() => junction_lp() <&> bcast_starter())
      )
  );
};
