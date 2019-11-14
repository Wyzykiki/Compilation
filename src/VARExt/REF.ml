type function_definition = {
  name: string;
  code: FUNInstr.sequence;
  parameters: (string * bool) list;
  locals: (string * int) list;
}
    
type program = {
  text: function_definition list;
  globals: (string * int) list
}
