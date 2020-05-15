#!/bin/bash

for filename in testcases/*.asm; do
  echo "Assembly of $filename";
  ./cli.pl assemble $filename "testcases_assembled/$(basename $filename .asm).out"
done

echo 'A' | ./cli.pl run_text testcases/basic.asm