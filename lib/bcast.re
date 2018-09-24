open Lwt.Infix;

let bcast = Logs.Src.create("bcast", ~doc="Broadcast traffic repeater");
module Log = (val (Logs_lwt.src_log(bcast): (module Logs_lwt.LOG)));

let no_broadcast = () => Lwt.return_unit;

let open_fifo = name => Lwt_unix.openfile(name, [Unix.O_RDONLY], 0o640);

let read_len = (fd, buf, len) => {
  let rec read = (cnt, buf) =>
    if (cnt == len) {
      Lwt.return_unit;
    } else {
      Lwt_cstruct.read(fd, buf)
      >>= (rlen => read(cnt + rlen, Cstruct.shift(buf, rlen)));
    };
  read(0, buf);
};

let extract_bcast_pkt = (fd, hd, ()) =>
  Lwt.catch(
    () =>
      read_len(fd, hd, 2)
      >>= (
        () =>
          Lwt.return @@
          Cstruct.BE.get_uint16(hd, 0)
          >>= (
            pkt_len => {
              let pkt = Cstruct.create(pkt_len);
              read_len(fd, pkt, pkt_len) >>= (() => Lwt.return_some(pkt));
            }
          )
      ),
    exn =>
      Log.err(m => m("extract_bcast_pkt %s", Printexc.to_string(exn)))
      >>= (() => Lwt.return_none),
  );

let create = (~fifo=?, interfaces) =>
  switch (fifo) {
  | None => Lwt.return(no_broadcast)
  | Some(fname) =>
    open_fifo(fname)
    >>= (
      fd => {
        let hd = Cstruct.create(2);
        let pkt_stm = Lwt_stream.from(extract_bcast_pkt(fd, hd));
        let rec relay_lp = () =>
          Lwt_stream.get(pkt_stm)
          >>= (
            fun
            | Some(pkt) =>
              Log.debug(m =>
                m("got one broadcast pkt: %d", Cstruct.len(pkt))
              )
              >>= (
                () => {
                  Interfaces.relay_bcast(interfaces, pkt);
                  relay_lp();
                }
              )
            | None => Lwt.return_unit
          );
        Lwt.return(relay_lp);
      }
    )
  };
