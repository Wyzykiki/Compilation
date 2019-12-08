type function_definition = {
  name: string;
  code: RECInstr.sequence;
  parameters: string list;
  locals: (string * int) list;
}

type class_definition = {
  class_name: string;
  attributes: string list;
  methods: function_definition list;
  parent: string;
}

type program = {
  classes: class_definition list;
  functions: function_definition list;
  globals: (string * int) list;
}
