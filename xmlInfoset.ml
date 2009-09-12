(*
 * (c) 2007-2009 Anastasia Gornostaeva <ermine@ermine.pp.ru>
 *
 * W3C Recommendation 4 February 2004 
 * http://www.w3.org/TR/xml-infoset
 *)

type ucs4 = int
type 'a property_value =
  | NoValue
  | Unknown
  | Value of 'a

module type InfoItem =
sig
  type t
end
  
module rec Document : InfoItem =
struct
  type t = {
    children : child list;
    document_element : Element.t;
    notation : Notation.t list;
    unparsed_entities : UnparsedEntity.t list;
    base_URI : string;
    character_encoding_schema : string;
    standalone : bool property_value;
    version : string;
    all_declarations_processed : bool property_value
  }
  and child =
    | Element of Element.t
    | PI of ProcessingInstructions.t
    | Comment of Comment.t
end
    
and Element : InfoItem =
struct
  type t = {
    namespace_name : string property_value;
    local_name : string;
    prefix : string property_value;
    children : child list;
    attributes : Attribute.t list;
    namespace_attribute : Attribute.t list;
    in_scope_namespaces : Namespace.t list;
    base_URI : string property_value;
    parent : parent
  }
  and child =
    | Element of Element.t
    | PI of ProcessingInstructions.t
    | UnexpandedEntity of UnexpandedEntityReference.t
    | Character of Character.t
    | Comment of Comment.t
  and parent = Document of Document.t | Element of t
end

and Attribute : InfoItem =
struct
  type attribute_type =
    | ID
    | IDREF
    | IDREFS
    | ENTITY
    | ENTITIES
    | NMTOKEN
    | NMTOKENS
    | NOTATION
    | CDATA
    | ENUMERATION
        
  type t = {
    namespace_name : string property_value;
    local_name : string;
    prefix : string property_value;
    normalized_value : string;
    specified : bool;
    attribute_type : attribute_type property_value;
    references : reference list property_value;
    owner_element : Element.t
  }
  and reference =
    | Element of Element.t
    | UnparsedEntity of UnparsedEntity.t
    | Notation of Notation.t
end

and ProcessingInstructions : InfoItem =
struct
  type t = {
    target : string;
    content : string;
    base_URI : string property_value;
    notation : Notation.t property_value;
    parent : parent
  }
  and parent =
    | Document of Document.t
    | Element of Element.t
    | DTD of DTD.t
end

and UnexpandedEntityReference : InfoItem =
struct
  type t = {
    name : string;
    system_identifier : string property_value;
    public_identifier : string property_value;
    declaration_base_URI : string property_value;
    parent : Element.t
  }
end
  

and Character : InfoItem =
struct
  type t = {
    character_code : ucs4;
    element_content_whitespace : bool;
    parent : Element.t
  }
end

and Comment : InfoItem =
struct
  type t = {
    content : string;
    parent : parent
  }
  and parent = Document of Document.t | Element of Element.t
end

and DTD : InfoItem =
struct
  type t = {
    system_identifier : string property_value;
    public_identifier : string property_value;
    children : ProcessingInstructions.t list;
    parent : Document.t
  }
end

and UnparsedEntity : InfoItem =
struct
  type t = {
    name : string;
    system_identifier : string property_value;
    public_identifier : string property_value;
    declaration_base_URI : string property_value;
    notation_name : string;
    notation : Notation.t property_value
  }
end

and Notation : InfoItem =
struct
  type t = {
    name : string;
    system_identifier : string property_value;
    public_identifier : string property_value;
    declaration_base_URI : string property_value;
  }
end

and Namespace : InfoItem =
struct
  type t = {
    prefix : string property_value;
    namespace_name : string
  }
end

  
