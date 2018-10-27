open Core;
open Async;
open Cohttp_async;

/* Based off of https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/hello_world.ml */

let toStrings = values =>
  List.filter_map(
    values,
    ~f=
      fun
      | `String(item) => Some(item)
      | _ => None,
  );

let parseResponse = json =>
  switch (json) {
  | `List([`List(keys), `List(values)]) =>
    List.zip(toStrings(keys), toStrings(values))
    |> Option.bind(
         ~f=
           List.find(
             ~f=
               fun
               | (key, _) => key == "EST",
           ),
       )
    |> Option.map(~f=snd)
  | _ => None
  };

let apiUrl = Uri.of_string("https://api.census.gov/data/2013/language");

/* TODO: map codes to states and langs and take from query params */
let buildUri = () =>
  Uri.add_query_params(
    apiUrl,
    [
      ("get", ["NAME,EST,LANLABEL"]),
      ("for", ["county:047"]),
      ("in", ["state:36"]),
      ("LAN", ["607"]),
    ],
  );

let getJson = uri =>
  Client.get(uri)
  >>= (((_, body)) => Body.to_string(body))
  >>| Yojson.Safe.from_string;

let getCount = () => buildUri() |> getJson >>| parseResponse;

let handler = (~body as _, _sock, req) => {
  let uri = Cohttp.Request.uri(req);
  switch (Uri.path(uri)) {
  | "/test" =>
    getCount()
    >>= (
      fun
      | None => Server.respond_string(~status=`Not_found, "not found")
      | Some(result) => Server.respond_string(result)
    )
  | _ => Server.respond_string(~status=`Not_found, "Route not found")
  };
};

let start_server = (port, ()) => {
  Caml.Printf.eprintf("Listening for HTTP on port %d\n", port);
  Caml.Printf.eprintf("Try 'curl http://localhost:%d/test'\n%!", port);
  Server.create(
    ~on_handler_error=`Raise,
    Async_extra.Tcp.Where_to_listen.of_port(port),
    handler,
  )
  >>= (_ => Deferred.never());
};

let run = () => {
  module Command = Async_extra.Command;
  Command.async_spec(
    ~summary="Start a hello world Async server",
    Command.Spec.(
      empty
      +> flag(
           "-p",
           optional_with_default(8080, int),
           ~doc="int Source port to listen on",
         )
    ),
    start_server,
  )
  |> Command.run;
};
