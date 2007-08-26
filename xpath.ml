type qname = string * string

type step = [
| `AxisStep
| `FilterStep
]


type primary_type =
   | String of string
   | Integer of int
   | Decimal of float
   | Double of float
   | Boolean of bool
   | VarRef of qname

