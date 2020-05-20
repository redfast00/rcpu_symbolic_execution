#!/bin/bash

for filename in testcases/*.asm; do
  echo -e "\e[32mAssembly of ${filename}\e[0m";
  ./cli.pl assemble $filename "testcases_assembled/$(basename $filename .asm).out"
done

# The files in testcases were assembled with the Python RCPU assembler, these
#  should be correct and are then compared against the files assembled by this assembler
sha1sum testcases/basic.out testcases_assembled/basic.out
sha1sum testcases/win_label.out testcases_assembled/win_label.out

# Should output A + 1, so B
# Parse assembly in text format and run it
echo "\e[32mRunning interpreter from source code\e[0m"
echo 'A' | ./cli.pl run_text testcases/basic.asm

echo -e "\n\n\e[32mRunning interpreter from assembled binary\e[0m"
# Parse assembly in binary format and run it
echo 'B' | ./cli.pl run_binary testcases/basic.out

echo -e "\n\n\e[32mRunning constraint tests\e[0m"
# Find an input for which the output is even
./cli.pl even testcases_assembled/even.out

# Find an input that results in a certain register config at program end
./cli.pl register_config testcases_assembled/register_conf.out

# Find an input that results in a certain instruction getting executed
./cli.pl reach_ip testcases_assembled/win_label.out 10

# Find an input that results in a certain stack configuration
./cli.pl stack_config testcases_assembled/stack_config.out

# Find an input that results in the RCPU-machine crashing
./cli.pl crash testcases_assembled/crashing.out

echo -e "\e[32mThese next examples should fail, because there is no solution\e[0m"

# Hardcoded 1 as output
./cli.pl even testcases_assembled/trivial.out