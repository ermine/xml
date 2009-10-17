open Ocamlbuild_plugin
open Myocamlbuild_config

let _ =  dispatch begin function
  | After_rules ->
      extern "conversion";
      
      
      flag ["ocaml"; "pp"; "use_unicode.syntax"] &
        S[A"caml4pof"; A"./pa_unicode.cmo"];
      dep ["ocaml"; "ocamldep"; "use_unicode.syntax"]
        ["./pa_unicode.cmo"];

      install_lib "xml" []

  | _ ->
      ()
end
