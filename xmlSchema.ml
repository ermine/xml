(*
 * (c) 2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *
 * XML Schema Part 1: Structures Second Edition
 * http://www.w3.org/TR/xmlschema-1/
 *)

open Xml

let match_elements els list =
  let rec aux_match res els l =
    match els, l with
      | [], []  -> List.rev res
      | x :: xs, (ns, name, required) :: ls ->
          if x.ns = ns && x.name = name then
            aux_match (x :: res) xs ls
          else if l.required then
            raise Required l.name
          else
            aux_match res xs ls
      | _ ->
          raise Incomplete
  in
    aux_match [] els list

(*
 * 13 components
 *
 * The primary components, which may (type definitions) or must
 * (element and attribute declarations) have names are as follows:
 * 
 *    * Simple type definitions                                                  
 *    * Complex type definitions                                                 
 *    * Attribute declarations                                                   
 *    * Element declarations                                                     
 * 
 * The secondary components, which must have names, are as follows:             
 * 
 *    * Attribute group definitions                                              
 *    * Identity-constraint definitions                                          
 *    * Model group definitions                                                  
 *    * Notation declarations                                                    
 * 
 * Finally, the "helper" components provide small parts of other components;    
 * they are not independent of their context:
 *
 *    * Annotations                                                              
 *    * Model groups                                                             
 *    * Particles                                                                
 *    * Wildcards                                                                
 *    * Attribute Uses                                                           
 * 
 *)

let ns_xsd = "http://www.w3.org/2001/XMLSchema"

module Attribute =
struct
  (*
   * <attribute
   *   default = string
   *   fixed = string
   *   form = (qualified | unqualified)
   *   id = ID
   *   name = NCName
   *   ref = QName
   *   type = QName
   *   use = (optional | prohibited | required) : optional
   *   {any attributes with non-schema namespace . . .}>
   *   Content: (annotation?, simpleType?)
   * </attribute>
   *)
  type t = {
    name : Xml.ncname;
    target_namespace : Xml.namespace option;
    type_definition : SimpleType.t;
    scope : scope option; (* either Globar or Complex type definition *)
    value_constraint : value_constraint option;
    annotation : Annotation.t option
  }

  let find_elements el list =
    List.fold_left (acc el ->
                      if el.ns =


                        
  let decode parent el =
    let annotation, simple_type_child =
      find_elements el.children [ns_xsd, "annotation"; ns_xsd, "simpleType"] in
    let name = Xml.get_attr_value "name" el in
    let target_namespace = schema.target_namespace in
    let type_definition =
      match simple_type_child with
        | None -> (
            try get_attr_value "type" el
            with Not_found -> simple_ur_type
          )
        | Some t -> in
    let scope = Global in
    let value_constraint = 
      try let v = get_attr_value "default" el in
        Some (Default v)
      with Not_found ->
        try let v = get_attr_value "fixed" el in
          Some (Fixed v)
        with Not_found -> None in
      { name = name;
        target_namespace = target_namespace;
        type_definition = type_definition;
        scope = scope;
        value_constraint = value_constraint;
        annotation = annotation
      }
      
    
