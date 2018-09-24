type t;

let create: (Interfaces.t, Nat.t) => t;

/** [connect t name_x name_y] enables communication between service [name_x] and service [name_y]. */

let connect: (t, string, string) => Lwt.t(unit);

/** [disconnect t name ip] disables the communication between service [name] and all the others,
    [ip] is [name]'s IP address. */

let disconnect: (t, string, Ipaddr.V4.t) => Lwt.t(unit);

let allow_privileged_ip: (t, Ipaddr.V4.t) => Lwt.t(unit);

let allow_privileged_host: (t, string) => Lwt.t(unit);

let allow_privileged_network: (t, Ipaddr.V4.Prefix.t) => Lwt.t(unit);

let disallow_privileged_network: (t, Ipaddr.V4.Prefix.t) => Lwt.t(unit);

/** [is_authorized_resolve t src_ip name] tries to decide if a query from [src_ip] for the service [name]
    is allowed, if ok, a tuple of src_ip and resolved address of [name] is returned, otherwise it's just the [src_ip]. */

let is_authorized_resolve:
  (t, Ipaddr.V4.t, string) =>
  Lwt.t(result((Ipaddr.V4.t, Ipaddr.V4.t), Ipaddr.V4.t));

/** [is_authorized_transport t src_ip dst_ip] tries to decide if a packet with [src_ip] and [dst_ip] is
    allowed to be forwarded. */

let is_authorized_transport: (t, Ipaddr.V4.t, Ipaddr.V4.t) => bool;

let substitute: (t, string, Ipaddr.V4.t, Ipaddr.V4.t) => Lwt.t(unit);
