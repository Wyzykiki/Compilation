#!/bin/bash
ocamlbuild -clean
ocamlbuild src/VM.native
ocamlbuild src/Assembler.native
ocamlbuild src/STK/STKCompiler.native
ocamlbuild src/STK/STKCompilerAcc.native
ocamlbuild src/STK/STKCompilerAlloc.native
ocamlbuild src/ART/ARTCompiler.native
ocamlbuild src/IMP/IMPCompiler.native
ocamlbuild src/VARInterpreter/VARInterpreter.native
ocamlbuild src/VAR/VARCompiler.native