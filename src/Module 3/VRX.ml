type function_definition = {
  name: string;
  code: VRXInstr.sequence;
  parameters: string list;
}
    
type program = {
  text: function_definition list;
  globals: (string * int) list
}
