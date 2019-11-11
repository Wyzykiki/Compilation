type function_definition = {
  name: string;
  code: VAR2Instr.sequence;
  parameters: string list;
}
    
type program = {
  text: function_definition list;
  globals: (string * int) list
}
