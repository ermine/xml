open Xmllexer

module Input =
struct
  open Lwt
  type 'a t = 'a Lwt.t

  type stream = char Lwt_stream.t
  let get s =
    Lwt_stream.peek s >>= function
      | Some c -> Lwt_stream.junk s >>= fun () -> return (Some c)
      | None -> return None

  module D = Decoder
    (struct type 'a t = 'a Lwt.t
            let fail = Lwt.fail
            let return = Lwt.return
            let (>>=) = Lwt.(>>=)
            type stream = char Lwt_stream.t
            let get = get end)

  exception UnknownEncoding
    
  let make_decoder encname =
    if encname = "UTF-8" then
      D.decode_utf8
    else
      raise UnknownEncoding
end

  
module LS = Xmllexer.LocatedStream (Lwt) (Input)
module M = Xmllexer_generic.Make
  (LS)
  (Xmllexer.Encoding)
  (Xmllexer.XmlStanza (Lwt))

let (>>=) = Lwt.(>>=)

let _ =
  Lwt_main.run (
    Lwt_io.open_file ~mode:Lwt_io.input Sys.argv.(1) >>= fun f ->
    let strm = Lwt_io.read_chars f in
    let strm = LS.make_stream strm in
    let next_token = M.make_lexer strm in
    let rec loop () = next_token () >>= function
      | Some _ -> loop ()
      | None -> Lwt.return ()
    in
      loop ()
  )
