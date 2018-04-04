#!/bin/bash
for i in output/*.s; do
	nasm -O1 -f elf -g -F dwarf "$i" 
done
ld -melf_i386 -o main output/*.o

