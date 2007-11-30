(*
 * (c) 2007 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *)

open Fstream

exception Illegal
exception TooFew
exception UnknownEncoding of string

type encoding = 
   | Enc_ASCII
   | Enc_Latin1 
   | Enc_UTF8
   | Enc_UTF16
   | Enc_UCS4

type byte_order =
   | BE
   | LE

let rec decode_ascii ch =
   match ch with
      | '\000'..'\127' ->
	   R (Char.code ch, decode_ascii)
      | _ ->
	   raise Illegal

let rec encode_ascii ucs4 =
   if ucs4 < 0x128 then
      R ([Char.chr ucs4], encode_ascii)
   else
      raise Illegal

let rec decode_latin1 ch =
   R (Char.code ch, decode_ascii)

let rec encode_latin1 ucs4 =
   if ucs4 < 0x100 then
      R ([Char.chr ucs4], encode_latin1)
   else
      raise Illegal

(*
 * UTF-8
 *)

let utf8_len = [|        (* Char byte length according to first UTF-8 byte. *)
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
   1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
   0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
   2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
   3; 3; 3; 3; 3; 3; 4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 
|]

let rec decode_utf8 c0 =
   let b0 = Char.code c0 in
      match utf8_len.(b0) with
	 | 0 ->
	      raise Illegal
	 | 1 ->
	      R (b0, decode_utf8)
	 | 2 ->
	      F (fun c1 ->
		    let b1 = Char.code c1 in
                       if b1 lsr 6 != 0b10 then 
			  raise Illegal
                       else
			  R (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F), 
			     decode_utf8)
		)
	 | 3 -> 
	      F (fun c1 ->
		    F (fun c2 ->
			  let b1 = Char.code c1
			  and b2 = Char.code c2 in
			     if (b1 lsr 6 != 0b10) || (b2 lsr 6 != 0b10) then 
				raise Illegal;
			     let b = 
				((b0 land 0x0f) lsl 12) lor 
				   ((b1 land 0x3f) lsl 6) lor (b2 land 0x3f)
			     in
				if (b >= 0xd800) && (b <= 0xdf00) then
				   raise Illegal;
				R (b, decode_utf8)
		      )
		)
	 | 4 ->
	      F (fun c1 ->
		    F (fun c2 ->
			  F (fun c3 ->
				let b1 = Char.code c1
				and b2 = Char.code c2
				and b3 = Char.code c3 in
				   if (b1 lsr 6 != 0b10) || 
				      (b2 lsr 6 != 0b10) || 
				      (b3 lsr 6 != 0b10) then
					 raise Illegal;
				   let b =
				      ((b0 land 0x07) lsl 18) lor 
					 ((b1 land 0x3f) lsl 12) 
				      lor ((b2 land 0x3f) lsl 6) lor 
					 (b3 land 0x3f)
				   in
				      R (b, decode_utf8)
			    )
		      )
		)
	 | _ -> assert false   
	   
let uarray_of_utf8_string str =
   let rec aux_decode acc f i =
      if i < String.length str then
	 match f str.[i] with
	    | R (b, f) ->
		 aux_decode (b::acc) f (i+1)
	    | F f ->
		 aux_decode acc f (i+1)
      else
	 Array.of_list (List.rev acc)
   in
      aux_decode [] decode_utf8 0

let rec encode_utf8 ucs4 =
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
      R ((List.map Char.chr bytes), encode_utf8)

(*
 * UTF-16
 * 
 * RFC 2781
 *)

let chars_of_bo bo =
   match bo with
      | BE -> ['\254'; '\255']
      | LE -> ['\255'; '\254']

let char_pair_of_ucs4 bo ucs4 = 
   match bo with
      | LE -> (Char.chr (ucs4 land 0xFF), Char.chr ((ucs4 lsr 8) land 0xFF ))
      | BE -> (Char.chr ((ucs4 lsr 8) land 0xFF), Char.chr (ucs4 land 0xFF))

let read2 bo f =
   fun c1 ->
      F (fun c2 ->
            let b1 = Char.code c1
            and b2 = Char.code c2 in
            let ucs4 =
               match bo with
                  | BE -> (b1 lsl 8) + b2
                  | LE -> b1 + (b2 lsl 8)
            in
               f ucs4
        )

let read4any f =
   fun c1 ->
      F (fun c2 ->
	    F (fun c3 ->
		  F (fun c4 -> f (Char.code c1) (Char.code c2) (Char.code c3) 
			(Char.code c4)
		    )))


let rec decode_utf16 bo =
   let f1 ucs4 =
      match ucs4 with
	 | 0xfeff -> F (decode_utf16 BE)
	 | 0xfffe -> F (decode_utf16 LE)
	 | _ ->
	      if ucs4 < 0xd800 || 0xdfff < ucs4 then (
		 R (ucs4, decode_utf16 bo)
	      ) else if ucs4 <= 0xdbff then
		 let f2 ucs42 =
		    if ucs42 < 0xdc00 || ucs42 > 0xdfff then
		       raise Illegal;
		    let upper10 = (ucs4 land 0x3ff) lsl 10
		    and lower10 = ucs42 land 0x3ff in
		       R (0x10000 + upper10 + lower10, decode_utf16 bo)
		 in
		    F (read2 bo f2)
	      else 
		 raise Illegal
   in
      read2 bo f1
	
let rec encode_utf16 bo =
   fun ucs4 ->
      if ucs4 < 0x10000 then
	 let (c1, c2) = char_pair_of_ucs4 bo ucs4 in
	    R ([c1; c2], encode_utf16 bo)
      else
	 let u' = ucs4 - 0x10000 in
	 let w1 = 0xd800 + (u' lsr 10)
	 and w2 = 0xdc00 + (u' land 0x3ff) in
	 let (c1,c2) = char_pair_of_ucs4 bo w1
	 and (c3,c4) = char_pair_of_ucs4 bo w2 in
	    R ([c1; c2; c3; c4], encode_utf16 bo)

(*
let fun_decode_utf16 = decode_utf16 BE

let fun_encode_utf16 =
   fun ucs4 ->
      match encode_utf16 BE ucs4 with
         | F f -> R (chars_of_bo BE, f)
         | R (r, f) -> R (chars_of_bo BE @ r, f)
*)

(*
  * UCS-4
 *)

let rec decode_ucs4 c1 =
   let f b1 b2 b3 b4 =
      let ucs4 = b1 lsl 24 + b2 lsl 16 + b3 lsl 8 + b1 in
	 if ucs4 <= 0x7fffffff then
	    R (ucs4, decode_ucs4)
	 else
	    raise Illegal
   in
      read4any f c1

let rec encode_ucs4 ucs4 =
   if ucs4 <= 0x7fffffff then
      let c1 = ucs4 lsr 24
      and c2 = (ucs4 lsr 16) land 0xFF
      and c3 = (ucs4 lsr 8) land 0xFF
      and c4 = ucs4 land 0xFF in
	 R ([Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4], encode_ucs4)
   else
      raise Illegal

let decode_ucs4be = decode_ucs4
let encode_ucs4be = encode_ucs4

let rec decode_ucs4le c1 =
   let f b1 b2 b3 b4 =
      let ucs4 = b4 lsl 24 + b3 lsl 16 + b2 lsl 8 + b1 in
	 if ucs4 <= 0x7fffffff then
	    R (ucs4, decode_ucs4le)
	 else
	    raise Illegal
   in
      read4any f c1

let rec encode_ucs4le ucs4 =
   if ucs4 <= 0x7fffffff then
      let c4 = ucs4 lsr 24
      and c3 = (ucs4 lsr 16) land 0xFF
      and c2 = (ucs4 lsr 8) land 0xFF
      and c1 = ucs4 land 0xFF in
	 R ([Char.chr c1; Char.chr c2; Char.chr c3; Char.chr c4], encode_ucs4)
   else
      raise Illegal
	 
let rec decode_2143 c1 =
   let f b1 b2 b3 b4 =
      let ucs4 = b2 lsl 24 + b1 lsl 16 + b4 lsl 8 + b3 in
	 R (ucs4, decode_2143)
   in
      read4any f c1

let rec decode_3412 c1 =
   let f b1 b2 b3 b4 =
      let ucs4 = 
	 b3 lsl 24 + b4 lsl 16 +b1 lsl 8 + b2 in
	 R (ucs4, decode_3412)
   in
      read4any f c1

let autodetect_encoding b0 b1 b2 b3 =
   match b0, b1, b2, b3 with
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
