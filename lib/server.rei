module Make:
  (B: Vnetif.BACKEND) =>
  {
    module Request = Opium_kernel.Rock.Request;
    module Response = Opium_kernel.Rock.Response;

    type conn = (
      Cohttp_mirage.Server_with_conduit.IO.conn,
      Cohttp.Connection.t,
    );
    type callback =
      (conn, Cohttp.Request.t, Cohttp_lwt_body.t) =>
      Lwt.t((Cohttp.Response.t, Cohttp_lwt_body.t));
    type t;

    let mac: t => Macaddr.t;

    let default_not_found: callback;
    let callback_of_routes:
      list(
        (Cohttp.Code.meth, Opium_kernel.Route.t, Opium_kernel.Rock.Handler.t),
      ) =>
      callback;
    let get:
      (string, Opium_kernel.Rock.Handler.t) =>
      (Cohttp.Code.meth, Opium_kernel.Route.t, Opium_kernel.Rock.Handler.t);
    let post:
      (string, Opium_kernel.Rock.Handler.t) =>
      (Cohttp.Code.meth, Opium_kernel.Route.t, Opium_kernel.Rock.Handler.t);

    let json_of_body_exn: Request.t => Lwt.t(Ezjsonm.t);

    type body = [
      | `Html(string)
      | `Json(Ezjsonm.t)
      | `Xml(string)
      | `String(string)
    ];
    let respond:
      (~headers: Cohttp.Header.t=?, ~code: Cohttp.Code.status_code=?, body) =>
      Response.t;
    let respond':
      (~headers: Cohttp.Header.t=?, ~code: Cohttp.Code.status_code=?, body) =>
      Lwt.t(Response.t);

    let make: (B.t, Ipaddr.V4.t) => Lwt.t(t);
    let start: (t, ~port: int=?, ~callback: callback=?, unit) => Lwt.t(unit);
  };
