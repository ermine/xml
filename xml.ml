exception XMLError of string

type namespace = [
| `URI of string
| `None
]

type prefix = string

type ncname = string

type ns_mapping = prefix * namespace

type qname = ns_mapping * ncname

type cdata = string

type external_id = [ `System of string | `Public of string * string ]

type dtd = {
   name : string;
   external_id : external_id option;
   intsybset : string
}

type node = 
   | Root of nodeset ref
   | Element of node ref
	* qname
	* nodeset ref
	* nodeset ref
	* nodeset ref
   | Attribute of node ref * qname * cdata
   | NS of node ref * ns_mapping
   | Text of node ref * cdata
   | Comment of node ref * cdata
   | ProcessingInstruction of node ref * ncname * cdata
and nodeset = node list

let default_ns = ("", `None)

let decode = Xml_decode.decode
let encode = Xml_encode.encode

let string_of_qname = function
   | (ns, localpart) when ns = default_ns -> localpart
   | ((prefix, uri), localpart) -> prefix ^ ":" ^ localpart

let string_of_list f sep list =
   match list with 
      | [] -> ""
      | x :: [] -> f x
      | x :: xs -> List.fold_left (fun res x -> res ^ sep ^ (f x)) (f x) xs

let string_of_attr : node -> string = function
   | Attribute (_parent, qname, value) ->
	(string_of_qname qname) ^ "='" ^ encode value ^ "'"
   | _ -> raise (XMLError "node is not an attribute in string_of_attr")

let string_of_ns = function
   | NS (p, (prefix, ns)) -> 
	(match ns with 
            | `None -> ""       
            | `URI str ->
		 if prefix = "" then
		    "xmlns='" ^ encode str ^ "'"
		 else
		    "xmlns:" ^ prefix ^ "='" ^encode  str ^ "'"
	)
   | _ -> ""

let rec serialize out = function
   | Root nodes ->
	List.iter (serialize out) !nodes
   | Element (parent, qname, nss, attrs, children) ->
	out "<";
	out (string_of_qname qname);
	if List.length !attrs != 0 then (
	   out " ";
	   out (string_of_list string_of_attr " " !attrs)
	);
	if List.length !nss != 0 then (
	   out " ";
	   out (string_of_list string_of_ns " " !nss)
	);
	if !children = [] then
	   out "/>"
        else (
           out ">";
           List.iter (serialize out) !children;
           out "</";
	   out (string_of_qname qname);
	   out ">"
        )
   | Text (parent, text) -> 
	out (encode text)
   | Comment (parent, comment) -> 
	out "<!-- ";
	out comment;
	out " -->"
   | ProcessingInstruction (parent, target, pi) -> 
	out "<?";
	out target;
	out " ";
	out pi;
	out "?>"
   | _ -> ()
