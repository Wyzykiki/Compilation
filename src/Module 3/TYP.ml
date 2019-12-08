type expr_type =
  | TInt
  | TBool
  | TPointer of expr_type
  | TArray of expr_type
  | TFun of expr_type * expr_type list
  | TRecord of string

type record_definition = {
  rec_name: string;
  fields: (string * expr_type) list;
}

type function_definition = {
  name: string;
  ret_type: expr_type;
  code: RECInstr.sequence;
  parameters: (string * expr_type) list;
  locals: (string * expr_type * int) list;
}

type program = {
  types: record_definition list;
  text: function_definition list;
  globals: (string * expr_type * int) list
}
