type record_definition = {
  parent: string option;
  rec_name: string;
  fields: string list;
}

type function_definition = {
  name: string;
  code: RECInstr.sequence;
  parameters: string list;
  locals: (string * int) list
}
    
type program = {
  types: record_definition list;
  text: function_definition list;
  globals: (string * int) list
}
