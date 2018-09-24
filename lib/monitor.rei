type starter = unit => Lwt.t(unit);

type intf_event = [ | `Up(Intf.t, Intf.starter) | `Down(string)];

let create: unit => Lwt.t((Lwt_stream.t(intf_event), starter));
