type t = {
  dev: string,
  ip: Ipaddr.V4.t,
  network: Ipaddr.V4.Prefix.t,
  mtu: int,
  /** Gateway address of the subnet {!val:t.network} */
  mutable gateway: option(Ipaddr.V4.t),
  /** A stream of IPv4 packets */
  recv_st: Lwt_stream.t(Cstruct.t),
  /** To send IPv4 packets through this interface */
  send_push: option(Cstruct.t) => unit,
  acquire_fake_ip: unit => Lwt.t(Ipaddr.V4.t),
  release_fake_ip: Ipaddr.V4.t => Lwt.t(unit),
  /** To get a list of fake IPs currently used by this interface */
  fake_ips: unit => list(Ipaddr.V4.t),
};

type starter = unit => Lwt.t(unit);

/** [create ~dev ~cidr] returns a handle on this specific interface, and a starter which could
    be used later to start sending/receiving packets from/to this interface. */

let create: (~dev: string, ~cidr: string) => Lwt.t((t, starter));

let set_gateway: (t, Ipaddr.V4.t) => unit;
