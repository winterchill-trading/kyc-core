open! Core

(* Base letters for the Latin-1 Supplement and Latin Extended-A letters that occur in
   European names. *)
let folds =
  [ "àáâãäåāăą", "a"
  ; "çćĉċč", "c"
  ; "ďđ", "d"
  ; "èéêëēĕėęě", "e"
  ; "ĝğġģ", "g"
  ; "ĥħ", "h"
  ; "ìíîïĩīĭįı", "i"
  ; "ĵ", "j"
  ; "ķ", "k"
  ; "ĺļľŀł", "l"
  ; "ñńņňŉ", "n"
  ; "òóôõöøōŏő", "o"
  ; "ŕŗř", "r"
  ; "śŝşš", "s"
  ; "ţťŧ", "t"
  ; "ùúûüũūŭůűų", "u"
  ; "ŵ", "w"
  ; "ýÿŷ", "y"
  ; "źżž", "z"
  ; "ß", "ss"
  ; "æ", "ae"
  ; "œ", "oe"
  ; "þ", "th"
  ; "ð", "d"
  ]
;;

let uchars s =
  let rec go i acc =
    if i >= String.length s
    then List.rev acc
    else (
      let d = Stdlib.String.get_utf_8_uchar s i in
      go (i + Stdlib.Uchar.utf_decode_length d) (Stdlib.Uchar.utf_decode_uchar d :: acc))
  in
  go 0 []
;;

let fold_table =
  let table = Hashtbl.create (module Int) in
  List.iter folds ~f:(fun (letters, base) ->
    List.iter (uchars letters) ~f:(fun u ->
      Hashtbl.set table ~key:(Uchar.to_scalar u) ~data:base;
      (* Upper-case forms sit 32 code points below in Latin-1 (À = à - 32) and one below
         in Latin Extended-A (Ā = ā - 1). *)
      let code = Uchar.to_scalar u in
      let upper = if code < 0x100 then code - 32 else code - 1 in
      if not (Hashtbl.mem table upper) then Hashtbl.set table ~key:upper ~data:base));
  table
;;

let stopwords =
  String.Set.of_list
    [ "mr"
    ; "mrs"
    ; "ms"
    ; "miss"
    ; "dr"
    ; "prof"
    ; "sir"
    ; "dame"
    ; "herr"
    ; "frau"
    ; "ltd"
    ; "llc"
    ; "inc"
    ; "plc"
    ; "gmbh"
    ; "ag"
    ; "sa"
    ; "sas"
    ; "sarl"
    ; "srl"
    ; "spa"
    ; "bv"
    ; "nv"
    ; "oy"
    ; "ab"
    ; "as"
    ; "co"
    ; "corp"
    ; "limited"
    ; "company"
    ]
;;

let tokens name =
  let buf = Buffer.create (String.length name) in
  List.iter (uchars name) ~f:(fun u ->
    let code = Uchar.to_scalar u in
    if code < 128
    then (
      let c = Char.of_int_exn code in
      if Char.is_alphanum c
      then Buffer.add_char buf (Char.lowercase c)
      else Buffer.add_char buf ' ')
    else (
      match Hashtbl.find fold_table code with
      | Some base -> Buffer.add_string buf base
      | None ->
        (* Combining marks (U+0300-U+036F) are dropped; other letters kept. *)
        if code >= 0x300 && code <= 0x36F
        then ()
        else (
          let b = Stdlib.Buffer.create 4 in
          Stdlib.Buffer.add_utf_8_uchar b u;
          Buffer.add_string buf (Stdlib.Buffer.contents b))));
  Buffer.contents buf
  |> String.split ~on:' '
  |> List.filter ~f:(fun t -> (not (String.is_empty t)) && not (Set.mem stopwords t))
;;

let normalize name = String.concat ~sep:" " (tokens name)

let jaro a b =
  let la = String.length a
  and lb = String.length b in
  if la = 0 && lb = 0
  then 1.0
  else if la = 0 || lb = 0
  then 0.0
  else (
    let window = Int.max 0 ((Int.max la lb / 2) - 1) in
    let ma = Array.create ~len:la false
    and mb = Array.create ~len:lb false in
    let matches = ref 0 in
    for i = 0 to la - 1 do
      let lo = Int.max 0 (i - window)
      and hi = Int.min (lb - 1) (i + window) in
      let j = ref lo in
      while !j <= hi do
        if (not mb.(!j)) && Char.equal a.[i] b.[!j]
        then (
          ma.(i) <- true;
          mb.(!j) <- true;
          incr matches;
          j := hi + 1)
        else incr j
      done
    done;
    if !matches = 0
    then 0.0
    else (
      let transpositions = ref 0
      and k = ref 0 in
      for i = 0 to la - 1 do
        if ma.(i)
        then (
          while not mb.(!k) do
            incr k
          done;
          if not (Char.equal a.[i] b.[!k]) then incr transpositions;
          incr k)
      done;
      let m = Float.of_int !matches in
      ((m /. Float.of_int la)
       +. (m /. Float.of_int lb)
       +. ((m -. (Float.of_int !transpositions /. 2.)) /. m))
      /. 3.))
;;

let jaro_winkler a b =
  let j = jaro a b in
  let max_prefix = Int.min 4 (Int.min (String.length a) (String.length b)) in
  let rec prefix i =
    if i < max_prefix && Char.equal a.[i] b.[i] then prefix (i + 1) else i
  in
  j +. (Float.of_int (prefix 0) *. 0.1 *. (1. -. j))
;;

let score a b =
  let ta = tokens a
  and tb = tokens b in
  let sorted ts = String.concat ~sep:" " (List.sort ts ~compare:String.compare) in
  Float.max
    (jaro_winkler (String.concat ~sep:" " ta) (String.concat ~sep:" " tb))
    (jaro_winkler (sorted ta) (sorted tb))
;;

let default_threshold = 0.85
let is_candidate ?(threshold = default_threshold) a b = Float.( >= ) (score a b) threshold
