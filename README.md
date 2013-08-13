
# What

This project aims to be an annotator/pretty-printer for machine code.

Eventually, you will be able to input a byte stream of machine code on standard
input, and it will print out the name of each instruction, the bits in each
sub-field, and what each field means.

This isn't really supposed to be a disassembler, but rather a tool that will
make a more easily readable version of the computer's own representation of the
code.


# Why

Though assemblers generally map their input to machine code in a one-to-one
manner, we typically ignore the code as the machine itself sees it, or as the
hardware designer sees it.  The curious student might like to match their
assembly with actual bytes that the computer interprets.  Normally you just
trust the assembler to do it, but if you're studying low-level details it may
be nice to reconcile the two representations.

I started this project when I was trying to learn about computer fundamentals
and historical computer systems.  I wanted to see how the assembler was
rendering its input to machine code, but it was too tedious to inspect it by
hand.  I wanted a tool that would automatically look up the bit fields in the
machine code tables for me so I didn't have to do it myself.


# Who

The intended audience is for people who want to experiment with writing machine
code from scratch.  They will be able to verify what they've written by running
it through this program and making sure they get back what they intended.

Also, this could be useful for people developing assemblers, to make sure their
assembler is outputting the machine code that they expect.


# How

The instruction set specifications are supposed to resemble the bit-by-bit
tables that they are usually described with.

The basic idea is that you write out a bit pattern for the instruction and how
it should be labeled, where each character is one bit:

    ( "1110 00 00 000 0", "NOP" )

(Spaces are ignored.)

If a given instruction contains some sub-field, you can specify the field with
strings of letters.  Each field must start with a capital letter, and must be
followed with lower-case letters.  The width of the field in bits must be
exactly equal to the number of characters in its field name.

    ( "000 IZOffsett", "AND" )

So this instruction contains three fields: one of length one, another of length
one, and a third of length seven.

Then the interpretations of each field are specified in a table as well:

    "Aaaaaa",  [0,1,1,0,0,0],  "(POP / [SP++])"

So this means for the field Aaaaaa, when it sees a 011000 bit pattern, label
that as (POP / [SP++]).


# Goals

One goal is to have a relatively simple syntax for adding new instruction sets
that mimics published instruction set tables.

Another goal is to focus on vintage instruction sets, because that's what I'm
interested in learning about right now.


# Running the program

For now, the input instruction stream is a hardcoded sequence of DCPU-16
instructions.  Run as

    runhaskell Main.hs


# Remarks

This isn't a disassembler, though I guess it's kind of similar.  This is more
about understanding the computer's machine code as-is.

The project is in the sketching / rapid expansion / iteration phase.  I'm also
a beginner Haskell programmer, so everything will inevitably go through a few
rounds of rewriting, refactoring and simplification.


# Author

Mark Zadel, 2013

GPLv3

