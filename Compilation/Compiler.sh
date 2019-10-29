#!/bin/bash
file=`basename $1 .var`
echo "VAR to IMP :"
./VARCompiler.native "${file}.var"
echo "IMP to ART :"
./IMPCompiler.native "${file}.imp"
echo "ART to STK :"
./ARTCompiler.native "${file}.art"
echo "STK to ASM :"
./STKCompilerAlloc.native "${file}.stk"
echo "ASM to BTC :"
./Assembler.native "${file}.asm"