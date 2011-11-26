(*
 * (c) 2007-2009 Anastasia Gornostaeva
 *)

exception Illegal
exception UnknownEncoding of string

type ucs4 = int

type t =
  | TooFew
  | Shift of int
  | Invalid
  | Result of int * ucs4

type byte_order =
  | BE
  | LE

type state = {
  bo: byte_order
}

let decode_ascii str i =
  match str.[i] with
    | '\000'..'\127' as ch ->
        Result (i+1, Char.code ch)
    | _ ->
        Invalid

let encode_ascii ucs4 =
  if ucs4 < 0x128 then
    [Char.chr ucs4]
  else
    raise Illegal

let decode_latin1 str i =
  Result (i+1, Char.code str.[i])

let encode_latin1 ucs4 =
  if ucs4 < 0x100 then
    [Char.chr ucs4]
  else
    raise Illegal

(*
 * UTF-8
 * 
 * RFC 3629
 *)

let decode_utf8 str i =
  match str.[i] with
    | '\000'..'\127' as ch ->
        Result (i+1, Char.code ch)
    | '\192'..'\223' as c1 ->
        if i+1 < String.length str then
          let n1 = Char.code c1 in
          let n2 = Char.code str.[i+1] in
            if (n2 lsr 6 != 0b10) then Invalid
            else Result (i+2, ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f))
        else
          TooFew
    | '\224'..'\239' as c1 ->
        if i+2 < String.length str then
          let n1 = Char.code c1
          and n2 = Char.code str.[i+1]
          and n3 = Char.code str.[i+2] in
            if (n2 lsr 6 != 0b10) || (n3 lsr 6 != 0b10) then
              Invalid
            else
              let p = 
                ((n1 land 0x0f) lsl 12) lor
                  ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
              in
                if (p >= 0xd800) && (p <= 0xdf00) then Invalid
                else Result (i+3, p)
        else
          TooFew
    | '\240'..'\247' as c1 ->
        if i+3 < String.length str then
          let n1 = Char.code c1
          and n2 = Char.code str.[i+1]
          and n3 = Char.code str.[i+2]
          and n4 = Char.code str.[i+3] in
            if (n2 lsr 6 != 0b10) ||
              (n3 lsr 6 != 0b10) || (n4 lsr 6 != 0b10) then
                Invalid
            else
              Result (i+4, 
                      (((n1 land 0x07) lsl 18) lor
                         ((n2 land 0x3f) lsl 12) lor
                         ((n3 land 0x3f) lsl 6) lor (n4 land 0x3f)))
        else
          TooFew
    | _ ->
        Invalid

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

let decode_utf16 bo =
  let bo = ref bo in
    fun str i ->
      if i+1 < String.length str then
        let b1 = Char.code str.[i]
        and b2 = Char.code str.[i+1] in
        let ucs4 =
          match !bo with
            | BE -> (b1 lsl 8) + b2
            | LE -> b1 + (b2 lsl 8)
        in
          match ucs4 with
            | 0xfeff -> bo := BE; Shift (i+2)
            | 0xfffe -> bo := LE; Shift (i+2)
            | _ -> 
                if ucs4 < 0xd800 || ucs4 > 0xdfff then
                  Result (i+2, ucs4)
                else if not (ucs4 >= 0xd800 && ucs4 <= 0xdbff) then
                  Invalid
                else
                  if i+3 < String.length str then
                    let b3 = Char.code str.[i+2]
                    and b4 = Char.code str.[i+3] in
                    let ucs42 =
                      match !bo with
                        | BE -> (b3 lsl 8) + b4
                        | LE -> b3 + (b4 lsl 8)
                    in
                      if ucs42 >= 0xdc00 && ucs42 <= 0xdfff then
                        let upper10 = (ucs4 land 0x3ff) lsl 10
                        and lower10 = ucs42 land 0x3ff in
                          Result (i+4, (0x10000 + upper10 + lower10))
                      else
                        Invalid
                  else
                    TooFew
      else
        TooFew

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

let decode_ucs4 str i =
  if i+3 < String.length str then
    let b1 = Char.code str.[i]
    and b2 = Char.code str.[i+1]
    and b3 = Char.code str.[1+2]
    and b4 = Char.code str.[i+3] in
    let ucs4 = b1 lsl 24 + b2 lsl 16 + b3 lsl 8 + b4 in
      if ucs4 <= 0x7fffffff then
        Result (i+4, ucs4)
      else
        Invalid
  else
    TooFew
      
let encode_ucs4 ucs4 =
  if ucs4 <= 0x7fffffff then
    let c1 = ucs4 lsr 24
    and c2 = (ucs4 lsr 16) land 0xFF
    and c3 = (ucs4 lsr 8) land 0xFF
    and c4 = ucs4 land 0xFF in
      [Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4]
  else
    raise Illegal

let decode_ucs4be = decode_ucs4

let encode_ucs4be = encode_ucs4
  
let decode_ucs4le str i =
  if i+3 < String.length str then
    let b1 = Char.code str.[i]
    and b2 = Char.code str.[i+1]
    and b3 = Char.code str.[i+2]
    and b4 = Char.code str.[i+3] in
    let ucs4 = b4 lsl 24 + b3 lsl 16 + b2 lsl 8 + b1 in
      if ucs4 <= 0x7fffffff then
        Result (i+4, ucs4)
      else
        Invalid
  else
    TooFew
      
let encode_ucs4le ucs4 =
  if ucs4 <= 0x7fffffff then
    let c4 = ucs4 lsr 24
    and c3 = (ucs4 lsr 16) land 0xFF
    and c2 = (ucs4 lsr 8) land 0xFF
    and c1 = ucs4 land 0xFF in
      [Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4]
  else
    raise Illegal

let decode_2143 str i =
  if i+3 < String.length str then
    let b1 = Char.code str.[i]
    and b2 = Char.code str.[i+1]
    and b3 = Char.code str.[i+2]
    and b4 = Char.code str.[i+3] in
    let ucs4 = b2 lsl 24 + b1 lsl 16 + b4 lsl 8 + b3 in
      Result (i+4, ucs4)
  else
    TooFew

let decode_3412 str i =
  if i+3 < String.length str then
    let b1 = Char.code str.[i]
    and b2 = Char.code str.[i+1]
    and b3 = Char.code str.[i+2]
    and b4 = Char.code str.[i+3] in
    let ucs4 = b3 lsl 24 + b4 lsl 16 +b1 lsl 8 + b2 in
      Result (i+4, ucs4)
  else
    TooFew
        
let chars_of_bo bo =
  match bo with
    | BE -> ['\254'; '\255']
    | LE -> ['\255'; '\254']

let autodetect_encoding c0 c1 c2 c3 =
  match Char.code c0, Char.code c1, Char.code c2, Char.code c3 with
    | 0x00, 0x00, 0xFE, 0xFF ->
        (* UCS-4, big-endian machine (1234 order) *)
        "UCS-4", decode_ucs4

    | 0xFF, 0xFE, 0x00, 0x00 ->
        (* UCS-4, little-endian machine (4321 order) *)
        "UCS-4LE", decode_ucs4le

    | 0x00, 0x00, 0xFF, 0xFE ->
        (* UCS-4, unusual octet order (2143) *)
        "2134", decode_2143

    | 0xFE, 0xFF, 0x00, 0x00 ->
        (* UCS-4, unusual octet order (3412) *)
        "3412", decode_3412

    | 0xFE, 0xFF, _, _ ->
        (* UTF-16, big-endian *)
        "UTF-16", decode_utf16 BE

    | 0xFF, 0xFE, _, _  ->
        (* UTF-16, little-endian *)
        "UTF-16", decode_utf16 BE

    | 0xEF, 0xBB, 0xBF, _ ->
        (* UTF-8 *)
        "UTF-8", decode_utf8

    | 0x00, 0x00, 0x00, 0x3C ->
        (* 1234 BE *)
        "UCS-4", decode_ucs4

    | 0x3C, 0x00, 0x00, 0x00 ->
        (* 4321 *)
        "UCS-4LE", decode_ucs4le

    | 0x00, 0x00, 0x3C, 0x00 ->
        (* 2143 *)
        "2143", decode_2143

    | 0x00, 0x3C, 0x00, 0x00 ->
        (*  3412 *)
        "3312", decode_3412

    | 0x00, 0x3C, 0x00, 0x3F ->
        "UTF-16", decode_utf16 BE

    | 0x3C, 0x00, 0x3F, 0x00 ->
        "UTF-16", decode_utf16 LE

    | 0x3C, 0x3F, 0x78, 0x6D ->
        "UTF-8", decode_utf8

    | 0x4C, 0x6F, 0xA7, 0x94 ->
        (* EBCDIC *)
        failwith "Unknown Encoding"

    | _, _, _, _ ->
        "UTF-8", decode_utf8
  
