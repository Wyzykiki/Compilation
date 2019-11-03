#!/bin/bash
file=`basename $1 .var`
dir=`dirname $1`
echo "VAR to IMP :"
./VARCompiler.native "${dir}/${file}.var"
echo "IMP to ART :"
./IMPCompiler.native "${dir}/${file}.imp"
echo "ART to STK :"
./ARTCompiler.native "${dir}/${file}.art"
echo "STK to ASM :"
./STKCompilerAlloc.native "${dir}/${file}.stk"
echo "ASM to BTC :"
./Assembler.native "${dir}/${file}.asm"