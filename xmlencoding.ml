(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

exception Illegal
exception UnknownEncoding of string

type ucs4 = int

type byte_order =
  | BE
  | LE

type state = {
  bo: byte_order
}

type decoder = {
  need_bytes : char -> int;
  decode : char list -> ucs4 option
}

let make_decoder_ascii =
  {
    need_bytes = (function
                    | '\000'..'\127' -> 1
                    | _ -> raise Illegal);
    decode = (function
                | ['\000'..'\127' as ch] -> Some (Char.code ch)
                | _ -> raise Illegal)
  }

let encode_ascii ucs4 =
  if ucs4 < 0x128 then
    [Char.chr ucs4]
  else
    raise Illegal

let make_decoder_latin1 =
  {
    need_bytes = (fun _ -> 1);
    decode = (function
                | [ch] -> Some (Char.code ch)
                | _ -> raise Illegal)
  }

let encode_latin1 ucs4 =
  if ucs4 < 0x100 then
    [Char.chr ucs4]
  else
    raise Illegal

let make_decoder_utf8 =
  {
    need_bytes = (function
                    | '\000' .. '\127' -> 1
                    | '\192'..'\223' -> 2
                    | '\224'..'\239' -> 3
                    | '\240'..'\247' -> 4
                    | _ -> 0);
    decode =
      (function
         | ['\000'..'\127' as ch] ->
             Some (Char.code ch)
         | ['\192'..'\223' as c1; c2] ->
             let n1 = Char.code c1 in
             let n2 = Char.code c2 in
               if (n2 lsr 6 != 0b10) then raise Illegal;
               Some (((n1 land 0x1f) lsl 6) lor (n2 land 0x3f))
         | ['\224'..'\239' as c1; c2; c3] ->
             let n1 = Char.code c1 in
             let n2 = Char.code c2 in
             let n3 = Char.code c3 in
               if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then
                 raise Illegal
               else
                 let p = 
                   ((n1 land 0x0f) lsl 12) lor
                     ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
                 in
                   if (p >= 0xd800) && (p <= 0xdf00) then
                     raise Illegal
                   else
                     Some p
         | ['\240'..'\247' as c1; c2; c3; c4] ->
             let n1 = Char.code c1 in
             let n2 = Char.code c2 in
             let n3 = Char.code c3 in
             let n4 = Char.code c4 in
               if (n2 lsr 6 != 0b10) ||
                 (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10) then
                   raise Illegal
               else
                 Some (((n1 land 0x07) lsl 18) lor
                         ((n2 land 0x3f) lsl 12) lor
                         ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f))
         | _ -> raise Illegal)
  }

let encode_utf8 ucs4 =
  let bytes = 
    if ucs4 < 0x80 then
      [ucs4]
    else if ucs4 <= 0x7ff then
      [(0xc0 lor (ucs4 lsr 6)); (0x80 lor (ucs4 land 0x3f))]
    else if ucs4 <= 0xffff then (
      if (ucs4 >= 0xd800 & ucs4 < 0xe000) then 
        raise Illegal;
      [(0xe0 lor (ucs4 lsr 12));
       (0x80 lor ((ucs4 lsr 6) land 0x3f));
       (0x80 lor (ucs4 land 0x3f))
      ]
    )
    else if ucs4 <= 0x10ffff then
      [(0xf0 lor (ucs4 lsr 18));
       (0x80 lor ((ucs4 lsr 12) land 0x3f));
       (0x80 lor ((ucs4 lsr 6)  land 0x3f));
       (0x80 lor (ucs4 land 0x3f))]
    else 
      raise Illegal
  in
    List.map Char.chr bytes

(*
 * UTF-16
 *
 * RFC 2781
 *)

let make_decoder_utf16 bo =
  let bo = ref bo in
  let saved = ref None in
    {
      need_bytes = (function _ -> 2);
      decode =
        (function
           | [c1; c2] -> (
               let b1 = Char.code c1
               and b2 = Char.code c2 in
               let ucs4 =
                 match !bo with
                   | BE -> (b1 lsl 8) + b2
                   | LE -> b1 + (b2 lsl 8)
               in
                 match !saved with
                   | None -> (
                       match ucs4 with
                         | 0xfeff -> bo := BE; None
                         | 0xfffe -> bo := LE; None
                         | _ ->
                             if ucs4 < 0xd800 || ucs4 > 0xdfff then
                               Some ucs4
                             else if not (ucs4 >= 0xd800 && ucs4 <= 0xdbff) then
                               raise Illegal
                             else (
                               saved := Some ucs4;
                               None
                             )
                     )
                   | Some w1 ->
                       if ucs4 >= 0xdc00 && ucs4 <= 0xdfff then
                         let upper10 = (w1 land 0x3ff) lsl 10
                         and lower10 = ucs4 land 0x3ff in
                           Some (0x10000 + upper10 + lower10)
                       else
                         raise Illegal
             )
           | _ ->
               raise Illegal
        )
    }

let char_pair_of_ucs4 bo ucs4 =
  match bo with
    | LE -> (Char.chr (ucs4 land 0xFF), Char.chr ((ucs4 lsr 8) land 0xFF ))
    | BE -> (Char.chr ((ucs4 lsr 8) land 0xFF), Char.chr (ucs4 land 0xFF))

let encode_utf16 bo ucs4 =
  if ucs4 < 0x10000 then
    let (c1, c2) = char_pair_of_ucs4 bo ucs4 in
      [c1; c2]
  else
    let u' = ucs4 - 0x10000 in
    let w1 = 0xd800 + (u' lsr 10)
    and w2 = 0xdc00 + (u' land 0x3ff) in
    let (c1,c2) = char_pair_of_ucs4 bo w1
    and (c3,c4) = char_pair_of_ucs4 bo w2 in
      [c1; c2; c3; c4]

(*
 * UCS-4
 *)

let make_decoder_ucs4 =
  {
    need_bytes = (fun _ -> 4);
    decode = (function
                | [c1; c2; c3; c4] ->
                    let b1 = Char.code c1
                    and b2 = Char.code c2
                    and b3 = Char.code c3
                    and b4 = Char.code c4 in
                    let ucs4 = b1 lsl 24 + b2 lsl 16 + b3 lsl 8 + b4 in
                      if ucs4 <= 0x7fffffff then
                        Some ucs4
                      else
                        raise Illegal
                | _ ->
                    raise Illegal
             )
  }

let encode_ucs4 ucs4 =
  if ucs4 <= 0x7fffffff then
    let c1 = ucs4 lsr 24
    and c2 = (ucs4 lsr 16) land 0xFF
    and c3 = (ucs4 lsr 8) land 0xFF
    and c4 = ucs4 land 0xFF in
      [Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4]
  else
    raise Illegal

let make_decoder_ucs4be = make_decoder_ucs4

let encode_ucs4be = encode_ucs4
  
let make_decoder_ucs4le =
  {
    need_bytes = (fun _ -> 4);
    decode = (function
                | [c1; c2; c3; c4] ->
                    let b1 = Char.code c1
                    and b2 = Char.code c2
                    and b3 = Char.code c3
                    and b4 = Char.code c4 in
                    let ucs4 = b4 lsl 24 + b3 lsl 16 + b2 lsl 8 + b1 in
                      if ucs4 <= 0x7fffffff then
                        Some ucs4
                      else
                        raise Illegal
                | _ ->
                      raise Illegal
             )
  }

let encode_ucs4le ucs4 =
  if ucs4 <= 0x7fffffff then
    let c4 = ucs4 lsr 24
    and c3 = (ucs4 lsr 16) land 0xFF
    and c2 = (ucs4 lsr 8) land 0xFF
    and c1 = ucs4 land 0xFF in
      [Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4]
  else
    raise Illegal

let make_decoder_2143 =
  {
    need_bytes = (fun _ -> 4);
    decode = (function
                | [c1; c2; c3; c4] ->
                    let b1 = Char.code c1
                    and b2 = Char.code c2
                    and b3 = Char.code c3
                    and b4 = Char.code c4 in
                    let ucs4 = b2 lsl 24 + b1 lsl 16 + b4 lsl 8 + b3 in
                      Some ucs4
                | _ ->
                    raise Illegal
             )
  }

let make_decoder_3412 =
  {
    need_bytes = (fun _ -> 4);
    decode = (function
                | [c1; c2; c3; c4] ->
                    let b1 = Char.code c1
                    and b2 = Char.code c2
                    and b3 = Char.code c3
                    and b4 = Char.code c4 in
                    let ucs4 = b3 lsl 24 + b4 lsl 16 +b1 lsl 8 + b2 in
                      Some ucs4
                | _ ->
                    raise Illegal
             )
  }
        
let chars_of_bo bo =
  match bo with
    | BE -> ['\254'; '\255']
    | LE -> ['\255'; '\254']

let autodetect_encoding c0 c1 c2 c3 =
  match Char.code c0, Char.code c1, Char.code c2, Char.code c3 with
    | 0x00, 0x00, 0xFE, 0xFF ->
        (* UCS-4, big-endian machine (1234 order) *)
        "UCS-4", make_decoder_ucs4

    | 0xFF, 0xFE, 0x00, 0x00 ->
        (* UCS-4, little-endian machine (4321 order) *)
        "UCS-4LE", make_decoder_ucs4le

    | 0x00, 0x00, 0xFF, 0xFE ->
        (* UCS-4, unusual octet order (2143) *)
        "2134", make_decoder_2143

    | 0xFE, 0xFF, 0x00, 0x00 ->
        (* UCS-4, unusual octet order (3412) *)
        "3412", make_decoder_3412

    | 0xFE, 0xFF, _, _ ->
        (* UTF-16, big-endian *)
        "UTF-16", make_decoder_utf16 BE

    | 0xFF, 0xFE, _, _  ->
        (* UTF-16, little-endian *)
        "UTF-16", make_decoder_utf16 BE

    | 0xEF, 0xBB, 0xBF, _ ->
        (* UTF-8 *)
        "UTF-8", make_decoder_utf8

    | 0x00, 0x00, 0x00, 0x3C ->
        (* 1234 BE *)
        "UCS-4", make_decoder_ucs4

    | 0x3C, 0x00, 0x00, 0x00 ->
        (* 4321 *)
        "UCS-4LE", make_decoder_ucs4le

    | 0x00, 0x00, 0x3C, 0x00 ->
        (* 2143 *)
        "2143", make_decoder_2143

    | 0x00, 0x3C, 0x00, 0x00 ->
        (*  3412 *)
        "3312", make_decoder_3412

    | 0x00, 0x3C, 0x00, 0x3F ->
        "UTF-16", make_decoder_utf16 BE

    | 0x3C, 0x00, 0x3F, 0x00 ->
        "UTF-16", make_decoder_utf16 LE

    | 0x3C, 0x3F, 0x78, 0x6D ->
        "UTF-8", make_decoder_utf8

    | 0x4C, 0x6F, 0xA7, 0x94 ->
        (* EBCDIC *)
        failwith "Unknown Encoding"

    | _, _, _, _ ->
        "UTF-8", make_decoder_utf8
  
