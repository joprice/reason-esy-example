open Core;
open Async;
open Cohttp_async;

/* Based off of https://github.com/mirage/ocaml-cohttp/blob/master/examples/async/hello_world.ml

   Data / inspiration taken from

   https://github.com/grantwinney/15-apis-in-15-days/blob/0ce1087e64eca5ae5090839fcaac04921f7a32cb/Day%2013%20-%20US%20Census%20Bureau%20API.md
   https://api.census.gov/data/2013/language?get=NAME,EST,LANLABEL&for=county:020&in=state:02&LAN=625
   https://library.columbia.edu/locations/dssc/data/nycounty_fips.html
   https://www2.census.gov/geo/docs/reference/codes/files/st36_ny_cou.txt
   https://www.census.gov/geo/reference/codes/cou.html
   https://www.census.gov/hhes/socdemo/language/about/02_Primary_list.pdf
      */

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
