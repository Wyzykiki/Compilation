type function_definition = {
  name: string;
  code: FNXInstr.sequence;
  parameters: string list;
  locals: (string * int) list;
}
    
type program = {
  text: function_definition list;
  globals: (string * int) list;
}
