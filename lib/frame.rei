type t =
  | Ethernet{
      src: Macaddr.t,
      dst: Macaddr.t,
      payload: t,
    }
    : t
  | Arp{
      op: [ | `Request | `Reply | `Unknown],
      sha: Macaddr.t,
      spa: Ipaddr.V4.t,
      tha: Macaddr.t,
      tpa: Ipaddr.V4.t,
    }
    : t
  | Ipv4{
      src: Ipaddr.V4.t,
      dst: Ipaddr.V4.t,
      dnf: bool,
      ihl: int,
      raw: Cstruct.t,
      payload: t,
    }
    : t
  | Icmp{
      raw: Cstruct.t,
      payload: t,
    }
    : t
  | Udp{
      src: int,
      dst: int,
      len: int,
      raw: Cstruct.t,
      payload: t,
    }
    : t
  | Tcp{
      src: int,
      dst: int,
      syn: bool,
      raw: Cstruct.t,
      payload: t,
    }
    : t
  | Payload(Cstruct.t): t
  | Unknown: t;

/** [parse buffer] expects [buffer] to be an entire ethernet frame. */

let parse: Cstruct.t => Result.result(t, [ | `Msg(string)]);

/** [parse_ipv4_pkt buffer] expects [buffer] to be an IP packet. */

let parse_ipv4_pkt: Cstruct.t => Result.result(t, [ | `Msg(string)]);

let fr_info: t => string;
