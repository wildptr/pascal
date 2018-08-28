open Batteries
open Canon

type passive_stmt =
  | P_Assume of expr
  | P_Assert of expr
  | P_If of expr * passive_stmt list * passive_stmt list

type passive_proc = {
  body : passive_stmt list;
  vars : var array;
}
