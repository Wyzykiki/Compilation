type function_definition = {
  name: string;
  code: SEQInstr.instruction;
  parameters: string list;
}
    
type program = {
  text: function_definition list;
  globals: (string * int) list
}
