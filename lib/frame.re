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

open Result;
let (>>=) = (m, f) =>
  switch (m) {
  | Ok(x) => f(x)
  | Error(x) => Error(x)
  };

let need_space_for = (buf, n, description) =>
  if (Cstruct.len(buf) < n) {
    Error(
      `Msg(
        Printf.sprintf(
          "buffer is too short for %s: needed %d bytes but only have %d",
          description,
          n,
          Cstruct.len(buf),
        ),
      ),
    );
  } else {
    Ok();
  };

let parse_ipv4_pkt_exn = inner =>
  need_space_for(inner, 16, "IP datagram")
  >>= (
    () => {
      let vihl = Cstruct.get_uint8(inner, 0);
      let len = Cstruct.BE.get_uint16(inner, 1 + 1);
      let off = Cstruct.BE.get_uint16(inner, 1 + 1 + 2 + 2);
      let proto = Cstruct.get_uint8(inner, 1 + 1 + 2 + 2 + 2 + 1);
      let src =
        Cstruct.BE.get_uint32(inner, 1 + 1 + 2 + 2 + 2 + 1 + 1 + 2)
        |> Ipaddr.V4.of_int32;
      let dst =
        Cstruct.BE.get_uint32(inner, 1 + 1 + 2 + 2 + 2 + 1 + 1 + 2 + 4)
        |> Ipaddr.V4.of_int32;
      let dnf = off lsr 8 land 0x40 != 0;
      let ihl = vihl land 0xf;
      let raw = inner;
      need_space_for(inner, 4 * ihl, "IP options")
      >>= (
        () => {
          let inner = Cstruct.sub(inner, 4 * ihl, len - 4 * ihl);
          (
            switch (proto) {
            | 1 =>
              need_space_for(inner, 8, "ICMP message")
              >>= (
                () => {
                  let _ty = Cstruct.get_uint8(inner, 0);
                  let _code = Cstruct.get_uint8(inner, 1);
                  let _csum = Cstruct.BE.get_uint16(inner, 2);
                  let _id = Cstruct.BE.get_uint16(inner, 4);
                  let _seq = Cstruct.BE.get_uint16(inner, 6);
                  let payload = Cstruct.shift(inner, 8);
                  Ok(Icmp({raw: inner, payload: Payload(payload)}));
                }
              )
            | 6 =>
              need_space_for(inner, 14, "TCP header")
              >>= (
                () => {
                  let src = Cstruct.BE.get_uint16(inner, 0);
                  let dst = Cstruct.BE.get_uint16(inner, 2);
                  let offres = Cstruct.get_uint8(inner, 2 + 2 + 4 + 4);
                  let flags = Cstruct.get_uint8(inner, 2 + 2 + 4 + 4 + 1);
                  need_space_for(inner, offres lsr 4 * 4, "TCP options")
                  >>= (
                    () => {
                      let payload = Cstruct.shift(inner, offres lsr 4 * 4);
                      let syn = flags land 1 lsl 1 > 0;
                      Ok(
                        Tcp({
                          src,
                          dst,
                          syn,
                          raw: inner,
                          payload: Payload(payload),
                        }),
                      );
                    }
                  );
                }
              )
            | 17 =>
              need_space_for(inner, 8, "UDP header")
              >>= (
                () => {
                  let src = Cstruct.BE.get_uint16(inner, 0);
                  let dst = Cstruct.BE.get_uint16(inner, 2);
                  let len = Cstruct.BE.get_uint16(inner, 4);
                  let payload = Cstruct.shift(inner, 8);
                  let len = len - 8; /* subtract header length */
                  Ok(
                    Udp({
                      src,
                      dst,
                      len,
                      raw: inner,
                      payload: Payload(payload),
                    }),
                  );
                }
              )
            | _ =>
              Error(`Msg(Printf.sprintf("unrecognized proto: %d", proto)))
            }
          )
          /*Ok Unknown */
          >>= (payload => Ok(Ipv4({src, dst, dnf, ihl, raw, payload})));
        }
      );
    }
  );

let parse_ipv4_pkt = buf =>
  try (parse_ipv4_pkt_exn(buf)) {
  | exn => Error(`Msg(Printexc.to_string(exn)))
  };

let parse_arp_pkt = inner =>
  need_space_for(inner, 2, "ARP header")
  >>= (
    () => {
      let get_ha = (buf, offset) => {
        let raw = Cstruct.sub(buf, offset, 6);
        Macaddr.of_bytes_exn @@ Cstruct.to_string(raw);
      };

      let code = Cstruct.BE.get_uint16(inner, 6);
      let sha = get_ha(inner, 6 + 2);
      let spa = Cstruct.BE.get_uint32(inner, 6 + 2 + 6) |> Ipaddr.V4.of_int32;
      let tha = get_ha(inner, 6 + 2 + 6 + 4);
      let tpa =
        Cstruct.BE.get_uint32(inner, 6 + 2 + 6 + 4 + 6) |> Ipaddr.V4.of_int32;
      let op =
        switch (code) {
        | 1 => `Request
        | 2 => `Reply
        | _ => `Unknown
        };
      Ok(Arp({op, sha, spa, tha, tpa}));
    }
  );

let parse = buf =>
  try (
    need_space_for(buf, 14, "ethernet frame")
    >>= (
      () => {
        let ethertype = Cstruct.BE.get_uint16(buf, 12);
        let dst_option =
          Cstruct.sub(buf, 0, 6) |> Cstruct.to_string |> Macaddr.of_bytes;
        let src_option =
          Cstruct.sub(buf, 6, 6) |> Cstruct.to_string |> Macaddr.of_bytes;
        switch (dst_option, src_option) {
        | (None, _) =>
          Error(`Msg("failed to parse ethernet destination MAC"))
        | (_, None) => Error(`Msg("failed to parse ethernet source MAC"))
        | (Some(dst), Some(src)) =>
          let inner = Cstruct.shift(buf, 14);
          if (ethertype == 0x0800) {
            parse_ipv4_pkt(inner)
            >>= (payload => Ok(Ethernet({src, dst, payload})));
          } else if (ethertype == 0x0806) {
            parse_arp_pkt(inner)
            >>= (payload => Ok(Ethernet({src, dst, payload})));
          } else {
            Error(`Msg("Ethernet payload not ipv4 or arp pkt"));
          };
        };
      }
    )
  ) {
  | e =>
    Error(`Msg("Failed to parse ethernet frame: " ++ Printexc.to_string(e)))
  };

let rec fr_info =
  fun
  | Ethernet({src, dst, _}) =>
    Printf.sprintf(
      "Ethernet %s ->%s",
      Macaddr.to_string(src),
      Macaddr.to_string(dst),
    )
  | Arp({op}) =>
    Printf.sprintf(
      "Arp %s",
      switch (op) {
      | `Request => "request"
      | `Reply => "reply"
      | `Unknown => "unknown"
      },
    )
  | Ipv4({src, dst, payload}) => {
      let payload_str = fr_info(payload);
      Printf.sprintf(
        "Ipv4 %s -> %s {%s}",
        Ipaddr.V4.to_string(src),
        Ipaddr.V4.to_string(dst),
        payload_str,
      );
    }
  | Udp({src, dst, _}) => Printf.sprintf("Udp %d -> %d", src, dst)
  | Tcp({src, dst, _}) => Printf.sprintf("Tcp %d -> %d", src, dst)
  | Icmp(_) => "Icmp"
  | Payload(_) => "Payload"
  | Unknown => "Unknown";
