open Lwt.Infix;

module Ethif = Ethif.Make(Netif);
module Arpv4 = Arpv4.Make(Ethif, Mclock, OS.Time);

let intf = Logs.Src.create("intf", ~doc="Network Interface");
module Log = (val (Logs_lwt.src_log(intf): (module Logs_lwt.LOG)));

module Pkt = {
  let dst_of_ipv4 = buf => Ipv4_wire.get_ipv4_dst(buf) |> Ipaddr.V4.of_int32;

  let eth_hd = (source, destination, ethertype) =>
    Ethif_packet.(Marshal.make_cstruct({source, destination, ethertype}));
};

type t = {
  dev: string,
  ip: Ipaddr.V4.t,
  network: Ipaddr.V4.Prefix.t,
  mtu: int,
  mutable gateway: option(Ipaddr.V4.t),
  recv_st: Lwt_stream.t(Cstruct.t),
  send_push: option(Cstruct.t) => unit,
  acquire_fake_ip: unit => Lwt.t(Ipaddr.V4.t),
  release_fake_ip: Ipaddr.V4.t => Lwt.t(unit),
  fake_ips: unit => list(Ipaddr.V4.t),
};

let drop_pkt = (_: Cstruct.t) => Lwt.return_unit;

let read_intf = (dev, net, eth, arp, recv_push) => {
  let ipv6 = drop_pkt;
  let ipv4 = buf => {
    recv_push @@ Some(buf);
    Lwt.return_unit;
  };
  let arpv4 = Arpv4.input(arp);
  let listen_fn = Ethif.input(~arpv4, ~ipv4, ~ipv6, eth);
  Netif.listen(net, listen_fn)
  >>= (
    fun
    | Ok () => Log.info(m => m("%s disconnected!", dev))
    | Error(e) =>
      Log.warn(m => m("%s listen err: %a", dev, Netif.pp_error, e))
      >>= (
        () => Netif.disconnect(net) >>= (() => Lwt.return @@ recv_push(None))
      )
  );
};

let rec write_intf = (t, eth, arp, send_st) =>
  Lwt_stream.get(send_st)
  >>= (
    fun
    | Some(ipv4_pkt) => {
        let src_mac = Ethif.mac(eth);
        let dst = Pkt.dst_of_ipv4(ipv4_pkt);
        if (! Ipaddr.V4.Prefix.mem(dst, t.network) && t.gateway == None) {
          Log.err(m =>
            m(
              "%s(%a without gateway) nowhere to send pkt with dst:%a",
              t.dev,
              Ipaddr.V4.Prefix.pp_hum,
              t.network,
              Ipaddr.V4.pp_hum,
              dst,
            )
          )
          >>= (() => write_intf(t, eth, arp, send_st));
        } else if (dst == Ipaddr.V4.Prefix.broadcast(t.network)) {
          let dst_mac = Macaddr.broadcast;
          let hd = Pkt.eth_hd(src_mac, dst_mac, Ethif_wire.IPv4);
          Ethif.writev(eth, [hd, ipv4_pkt])
          >>= (
            fun
            | Ok () => Lwt.return_unit
            | Error(e) =>
              Log.err(m => m("%s Ethif.writev %a", t.dev, Ethif.pp_error, e))
          )
          >>= (() => write_intf(t, eth, arp, send_st));
        } else {
          let query_ip =
            if (Ipaddr.V4.Prefix.mem(dst, t.network)) {
              dst;
            } else {
              switch (t.gateway) {
              | None => assert(false)
              | Some(gw) => gw
              };
            };
          Arpv4.query(arp, query_ip)
          >>= (
            fun
            | Ok(dst_mac) => {
                let hd = Pkt.eth_hd(src_mac, dst_mac, Ethif_wire.IPv4);
                Ethif.writev(eth, [hd, ipv4_pkt])
                >>= (
                  fun
                  | Ok () => Lwt.return_unit
                  | Error(e) =>
                    Log.err(m =>
                      m("%s Ethif.writev %a", t.dev, Ethif.pp_error, e)
                    )
                );
              }
            | Error(e) =>
              Log.err(m => m("%s Arpv4.query: %a", t.dev, Arpv4.pp_error, e))
          )
          >>= (() => write_intf(t, eth, arp, send_st));
        };
      }
    | None => Log.warn(m => m("%s send stream is closed!", t.dev))
  );

let start_intf = (t, net, eth, arp, recv_push, send_st, ()) =>
  Lwt.catch(
    () =>
      try (
        Lwt.pick([
          read_intf(t.dev, net, eth, arp, recv_push),
          write_intf(t, eth, arp, send_st),
        ])
      ) {
      | e => Lwt.fail(e)
      },
    e => Log.err(m => m("%s on_intf: %s", t.dev, Printexc.to_string(e))),
  );

let init_stack = (dev, ip) =>
  Netif.connect(dev)
  >>= (
    net =>
      Mclock.connect()
      >>= (
        mclock => {
          let mtu = Netif.mtu(net);
          Ethif.connect(~mtu, net)
          >>= (
            ethif =>
              Arpv4.connect(ethif, mclock)
              >>= (
                arp =>
                  Arpv4.set_ips(arp, [ip])
                  >>= (() => Lwt.return((net, ethif, arp)))
              )
          );
        }
      )
  );

let lt_remove = e => List.filter(e' => e' != e);

let fake_ip_op = (arp, ip, network) => {
  let netmask = Ipaddr.V4.Prefix.bits(network);
  let last_added = {
    let network = Ipaddr.V4.Prefix.network(network);
    let net = Ipaddr.V4.to_int32(network);
    ref(Int32.(add(net, shift_left(one, 32 - netmask) |> pred)));
  };
  let returned = ref([]);
  (
    () => {
      let fake_ip =
        if (0 != List.length(returned^)) {
          let next = List.hd(returned^);
          returned := List.tl(returned^);
          next;
        } else {
          let next = Int32.(sub(last_added^, one));
          last_added := next;
          Ipaddr.V4.of_int32(next);
        };
      Arpv4.add_ip(arp, fake_ip) >>= (() => Lwt.return(fake_ip));
    },
    returned_ip => {
      returned := [returned_ip, ...returned^];
      let ips = Arpv4.get_ips(arp);
      let ips' = lt_remove(returned_ip, ips);
      Arpv4.set_ips(arp, ips') >>= (() => Lwt.return_unit);
    },
  );
};

type starter = unit => Lwt.t(unit);

let create = (~dev, ~cidr) => {
  let (network, ip) = Ipaddr.V4.Prefix.of_address_string_exn(cidr);
  init_stack(dev, ip)
  >>= (
    ((net, eth, arp)) => {
      let mtu = Netif.mtu(net);
      let (recv_st, recv_push) = Lwt_stream.create();
      let (send_st, send_push) = Lwt_stream.create();
      let (acquire_fake_ip, release_fake_ip) = fake_ip_op(arp, ip, network);
      let fake_ips = () => Arpv4.get_ips(arp) |> lt_remove(ip);
      let t = {
        dev,
        ip,
        network,
        gateway: None,
        mtu,
        recv_st,
        send_push,
        acquire_fake_ip,
        release_fake_ip,
        fake_ips,
      };
      Lwt.return((t, start_intf(t, net, eth, arp, recv_push, send_st)));
    }
  );
};

let set_gateway = (t, gw) => t.gateway = Some(gw);
