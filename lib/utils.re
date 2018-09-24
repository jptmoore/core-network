open Lwt.Infix;

module Log = {
  type log_threshold = ([ | `All | `Src(string)], Logs.level);

  module Log_config = Mirage_logs.Make(Pclock);

  let set_up_logs = logs => {
    let set_level = (~default, l) => {
      let srcs = Logs.Src.list();
      let default =
        try (
          snd @@
          List.find(
            fun
            | (`All, _) => true
            | _ => false,
            l,
          )
        ) {
        | Not_found => default
        };

      Logs.set_level(Some(default));
      List.iter(
        fun
        | (`All, _) => ()
        | (`Src(src), level) =>
          try (
            {
              let s = List.find(s => Logs.Src.name(s) == src, srcs);
              Logs.Src.set_level(s, Some(level));
            }
          ) {
          | Not_found =>
            Fmt.(pf(stdout))(
              "%a %s is not a valid log source.\n%!",
              Fmt.(styled(`Yellow, string)),
              "Warning:",
              src,
            )
          },
        l,
      );
    };

    Pclock.connect()
    >>= (
      pclock => {
        let reporter = Log_config.create(pclock);
        set_level(~default=Logs.Warning, logs);
        Log_config.set_reporter(reporter);
        Lwt.return_unit;
      }
    );
  };

  let log_threshold = {
    let enum = [
      ("error", Logs.Error),
      ("warning", Logs.Warning),
      ("info", Logs.Info),
      ("debug", Logs.Debug),
    ];
    let level_of_string = x =>
      try (List.assoc(x, enum)) {
      | Not_found => Fmt.kstrf(failwith, "%s is not a valid log level", x)
      };

    let string_of_level = x =>
      try (fst @@ List.find(((_, y)) => x == y, enum)) {
      | Not_found => "warning"
      };

    let parser = str =>
      switch (Astring.String.cut(~sep=":", str)) {
      | None => `Ok((`All, level_of_string(str)))
      | Some(("*", str)) => `Ok((`All, level_of_string(str)))
      | Some((src, str)) => `Ok((`Src(src), level_of_string(str)))
      };

    let serialize = ppf =>
      fun
      | (`All, l) => Fmt.string(ppf, string_of_level(l))
      | (`Src(s), l) => Fmt.pf(ppf, "%s:%s", s, string_of_level(l));

    (parser, serialize);
  };
};

module Containers = {
  type pair = (Ipaddr.V4.t, Ipaddr.V4.t);

  module IpPairMap =
    Map.Make({
      type t = pair;
      let compare = ((xx, xy), (yx, yy)) =>
        Ipaddr.V4.(
          if (compare(xx, yx) == 0 && compare(xy, yy) == 0) {
            0;
          } else if (0 != compare(xx, yx)) {
            compare(xx, yx);
          } else {
            compare(xy, yy);
          }
        );
    });

  module IpPairSet =
    Set.Make({
      type t = pair;
      let compare = ((xx, xy), (yx, yy)) =>
        Ipaddr.V4.(
          if (compare(xx, yx) == 0 && compare(xy, yy) == 0) {
            0;
          } else if (0 != compare(xx, yx)) {
            compare(xx, yx);
          } else {
            compare(xy, yy);
          }
        );
    });

  module IpMap =
    Map.Make({
      type t = Ipaddr.V4.t;
      let compare = Ipaddr.V4.compare;
    });

  module IpSet =
    Set.Make({
      type t = Ipaddr.V4.t;
      let compare = Ipaddr.V4.compare;
    });
};
