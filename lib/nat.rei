/** [Nat] does similar translations with NAT you may already know, but they are not exactly the same.
    Both the src and dst IP addresses of a packet will be translated here, from a pair of addresses
    belonging to some subnet A, to a pair of addresses belonging to another subnet B, thus giving
    the illusion of LAN communications between services while actually implementing seperation. */;

open Utils.Containers;

/** Inner fields:
    {ui
      {- translation: a map from tuple (src_ip, dst_ip) to tuple (nat_src_ip, nat_dst_ip). }
      {- rule_handles: a map from src_ip to list of (src_ip, dst_ip) tuples. }
    } */

type t;

let create: unit => t;

let add_rule: (t, pair, pair) => Lwt.t(unit);

let remove_rule: (t, pair) => Lwt.t(unit);

let get_rule_handles: (t, Ipaddr.V4.t) => Lwt.t(list(pair));

/** [translate t orig_ip_pair (pkt, fr)], [orig_ip_pair] is a tuple of src IP and dst IP addresses, extracted
    from [pkt], this function returns a four-element tuple of
    translated src IP, translated dst IP, a packet after translation is done, and the parsed packet. */

let translate:
  (t, pair, (Cstruct.t, Frame.t)) =>
  Lwt.t((Ipaddr.V4.t, Ipaddr.V4.t, Cstruct.t, Frame.t));

/** [get_translation t orig_ip_pair] returns a list of length either 1 or 0,
    depends on whether or not the corresponding translation rule exists in [t]. */

let get_translation: (t, pair) => list(pair);

let substitute: (t, Ipaddr.V4.t, Ipaddr.V4.t) => Lwt.t(unit);
