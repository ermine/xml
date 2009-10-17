(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 * 
 * To compare two unicode strings where one of them is known at compile time
 *)

open Camlp4

exception MalformedUTF8

module Id =
struct
  let name = "pa_unicode"
  let version = "1.0"
end

open Camlp4

module Unicode (Syntax : Sig.Camlp4Syntax) =
struct
  open Sig
  include Syntax
    

let length str =
  let rec aux_length i j =
    if i < String.length str then
      match String.unsafe_get str i with
        | '\000'..'\127' ->
            aux_length (succ i) (succ j)
        | '\192'..'\223' ->
            aux_length (i+2) (succ j)
        | '\224'..'\239' ->
            aux_length (i+3) (succ j)
        | '\240'..'\247' ->
            aux_length (i+4) (succ j)
        | _ ->
            raise MalformedUTF8
 else
   j
 in
  aux_length 0 0

let decode_utf8 str =
  let len = length str in
  let a = Array.create len 0 in
  let rec aux_decode i j =
    if i < String.length str then
      match String.unsafe_get str i with
        | '\000'..'\127' as ch ->
            let n = Char.code ch in
              a.(j) <- n;
              aux_decode (succ i) (succ j)
        | '\192'..'\223' as c1 ->
            let n1 = Char.code c1 in
            let n2 = Char.code str.[i+1] in
              if (n2 lsr 6 != 0b10) then raise MalformedUTF8
              else
                let n =  ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f) in
                  a.(j) <- n;
                  aux_decode (i+2) (succ j)
        | '\224'..'\239' as c1 ->
            let n1 = Char.code c1
            and n2 = Char.code str.[i+1]
            and n3 = Char.code str.[i+2] in
              if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then
                raise MalformedUTF8
              else
                let p = 
                  ((n1 land 0x0f) lsl 12) lor
                    ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
                in
                  if (p >= 0xd800) && (p <= 0xdf00) then raise MalformedUTF8
                  else (
                    a.(j) <- p;
                    aux_decode (i+3) (succ j)
                  )
        | '\240'..'\247' as c1 ->
            let n1 = Char.code c1
            and n2 = Char.code str.[i+1]
            and n3 = Char.code str.[i+2]
            and n4 = Char.code str.[i+3] in
              if (n2 lsr 6 != 0b10) ||
                (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10) then
                  raise MalformedUTF8
              else
                let n = (((n1 land 0x07) lsl 18) lor
                           ((n2 land 0x3f) lsl 12) lor
                           ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)) in
                  a.(j) <- n;
                  aux_decode (i+4) (succ j)
        | _ ->
            raise MalformedUTF8
    else
      a
  in
    aux_decode 0 0

  EXTEND Gram
      GLOBAL: patt expr;

    patt: LEVEL "simple"
      [ [ c = a_CHAR ->
            let u = Char.code c.[0] in
              <:patt< $`int:u$ >>
        | c1 = a_CHAR; ".."; c2 = a_CHAR ->
            let u1 = Char.code c1.[0]
            and u2 = Char.code c2.[0] in
            let rec gen_seq acc i =
              if i <= u2 then
                gen_seq (i::acc) (succ i)
              else
                List.rev acc
            in
            let pl =
              List.fold_right (fun l ls -> <:patt< $ls$ | $`int:l$>>)
                (gen_seq [] u1) <:patt<>> in
              <:patt< $pl$ >>
        ] ];
    expr: LEVEL "simple"
      [ [ c = a_CHAR ->
            let u = Char.code c.[0] in
              <:expr< $`int:u$ >>
        | c = a_CHAR ->
            let c = Char.code c.[0] in
              <:expr< $`int:c$ >>
        | s = a_STRING ->
            let pl =
              Array.fold_right (fun l ls -> <:expr< $`int:l$ :: $ls$ >>)
                (decode_utf8 s) <:expr< [] >>
            in
              <:expr< [ $pl$ ] >>
        ] ];
    END
end

let module M = Register.OCamlSyntaxExtension(Id)(Unicode) in ()
