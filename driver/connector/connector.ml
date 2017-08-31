open Lwt.Infix
open Mirage_types_lwt

let connector = Logs.Src.create "connector" ~doc:"Network Connector"
module Log = (val Logs_lwt.src_log connector : Logs_lwt.LOG)


module Make(N: NETWORK)(E: ETHIF)(Arp: ARP)(Ip: IPV4) = struct

  let hexdump_buf_debug desp buf =
    Log.debug (fun m ->
        let b = Buffer.create 128 in
        Cstruct.hexdump_to_buffer b buf;
        m "%s len:%d pkt:%s" desp (Cstruct.len buf) (Buffer.contents b))

  let drop_pkt (_: Cstruct.t) = Lwt.return_unit

  let is_ipv4_multicast buf =
    let dst = Cstruct.BE.get_uint32 buf 16 |> Ipaddr.V4.of_int32 in
    Ipaddr.V4.is_multicast dst

  let to_bridge conn buf =
    Lwt.catch
      (fun () ->
         if is_ipv4_multicast buf
         then Lwt.return_unit
         else
         Proto.Client.send conn buf
         (*>>= fun () -> hexdump_buf_debug "to_bridge" buf*))
      (fun e ->
         let msg = Printf.sprintf "to_bridge err: %s" @@ Printexc.to_string e in
         Log.err (fun m -> m "%s" msg) >>= fun () ->
         hexdump_buf_debug "to_bridge" buf)


  let rec from_bridge eth arp conn =
    Proto.Client.recv conn >>= fun buf ->
    (*hexdump_buf_debug "from_bridge" buf >>= fun () ->*)
    (*receving ip packet from bridge*)
    let dst_ipaddr = Cstruct.BE.get_uint32 buf 16 |> Ipaddr.V4.of_int32 in
    Arp.query arp dst_ipaddr >>= function
    | Ok destination ->
        let eth_hd =
          let source = E.mac eth in
          let ethertype = Ethif_wire.IPv4 in
          Ethif_packet.(Marshal.make_cstruct {source; destination; ethertype})
        in
        let buf = Cstruct.append eth_hd buf in
        (E.write eth buf >>= function
          | Ok () ->
              from_bridge eth arp conn
          | Error e ->
              Log.err (fun m -> m "from bridge E.write: %a" E.pp_error e))
    | Error e ->
        Log.err (fun m -> m "from bridge Arp.query: %a" Arp.pp_error e)


  let start nf eth arp ip =
    let socket_path = Key_gen.socket_path () in
    let intf = Key_gen.interface () in
    let macaddr = E.mac eth in
    let ip_addr, netmask =
      let addr = Key_gen.ip_address () in
      let prefix, ip = Ipaddr.V4.Prefix.of_address_string_exn addr in
      ip, Ipaddr.V4.Prefix.bits prefix in
    let endp = Proto.create_endp intf macaddr ip_addr netmask in

    Proto.Client.connect socket_path endp >>= function
    | Ok conn ->
        let listen_nf () =
          (*sendint ip packet to bridge, not ethernet frame*)
          let ipv4 = to_bridge conn in
          let arpv4 = Arp.input arp in
          let ipv6 = drop_pkt in
          let fn = E.input ~arpv4 ~ipv4 ~ipv6 eth in
          N.listen nf fn >>= function
          | Ok () ->
              Log.info (fun m -> m "to_bridge ok: %s" @@ Proto.endp_to_string endp)
          | Error e ->
              Log.err (fun m -> m "to_bridge err: %a" N.pp_error e)
        in
        Lwt.pick [
          listen_nf ();
          from_bridge eth arp conn;
        ] >>= fun () ->
        Proto.Client.disconnect conn
    | Error (`Msg msg) ->
        Log.err (fun m -> m "can't connect to %s: %s" socket_path msg)

end
