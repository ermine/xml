open Ocamlbuild_plugin
open Myocamlbuild_config

let _ =  dispatch begin function
  | After_rules ->
      extern "conversion";

      install_lib "xml" []

  | _ ->
      ()
end
