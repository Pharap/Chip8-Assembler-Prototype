# Chip8-Assembler-Prototype
Prototype of a Chip8 assembler. No manual, no documentation, no warranty.

## Safety Features

Labels are type-checked. `SYS` can only call system functions, `CALL` only works with functions and `JP` only works with regular labels.

## Missing Features

Comments. (Oops.)

## Rough Guide

All Chip8 instructions are implemented using the syntax listed at [Cowgod's Chip-8 Reference](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#3.1) with the exception that labels are used instead of addresses. (Hence system labels must be predeclared if you need them.)
As this is technically assembly, instructions are line-terminated rather than semicolon-terminated.

(Note: grammar syntax is askin to [WSN](https://en.wikipedia.org/wiki/Wirth_syntax_notation#WSN_defined_in_itself), but with single quotes instead of double quotes.)

### Labels

```
':'name':'
```

Example:
```
:start:
ADD V0 1
SE V0 10
JP start
```

### Directives

#### ascii

```
'ascii' [name] string
```

Dumps the ASCII encoding of the specified double-quoted `string` into memory at the current stream position.
`name` is optional, but necessary if you want to be able to actually refer to the block in question.
The primary use of omitting the `name` is to dump messages into the ROM without it being used directly in the program.

Chars can be accessed by using the `I` register. E.g. to read the nth char into `V0` (where `n` is stored in `V1`):
```
LD I name
ADD I V1
LD V0 [I]
```

You'll have to implement printing manually though.

#### data

```
'data' [name] '{' {byte} '}'
```

Much like `ascii`, but handles arbitray bytes, so it's more useful.
Can be used as a read-only array or to embed bytecode directly in the program, as an alternative to inline assembly.
Any data block can also be used as an array via the `I` pointer. (See char indexing example in `ascii`.)

Note: You can `JP` into a `data` block, but not `CALL` or `SYS` into one.

`byte`s may be an integer or a `#` followed by two hexadecimal digits.

#### system

```
'system' name address
```

Declares a system routine. Most Chip-8 emulators don't support the `SYS` instruction, but for those that do, this will allow you to use `SYS` by manually associating a label with an address. (This has the added advantage of encouraging people to document their system functions for future porters.)

`address`es may be an integer or a `#` followed by three hexadecimal digits.

#### macro

```
'macro' name '{' {statement} '}'
```

Declares a plain old text-subtitution style macro.
No parameters, no variables, just a verbatim dump.
(Though it's actually implemented by inserting into the parse tree rather than a true text replacement.)

(I considered adding parameters, but I'd have to change a bunch of the sub-parsers, so I've put it off until I can be bothered.)

Example (the infamous xor-swap):
```
macro swap_v0_vf
{
    XOR V0 VF
    XOR VF V0
    XOR V0 VF
}
```

#### expand

```
'expand' name
```

Dumps the contents of the macro named `name` into the instruction stream. Verbatim.

Example:
```
expand swap_v0_vf
```

#### block

```
'block' [name] '{' {statement} '}'
```

Blocks are a convenient way of labelling a sequence of statements. This can in turn be used to create loops.
Strictly speaking the label is positional so the terminating semicolon doesn't really signify the end of anything, it's more for improving readability and/or communicating intent.

The name is optional so the block can be used as a statement grouping construct without needing to provide a name.

#### function

```
'function' name '{' {statement} '}'
```

Functions are similar to blocks, but the label can only be branched to with `CALL` and a `RET` is automatically inserted at the end. (If you manually add one to the end, you'll end up with two. No dead code elimination here, this is assembly.)

Note that it's still possible to jump into a function by simply declaring a normal label at the start of the function because preventing that was more effort I couldn't be bothered to spend.

#### loop

```
'loop' '{' {statement} '}'
```

Loops are much like blocks, but a jump is automatically inserted at the end of the block of statements.
Loops are infinite, so they need to be broken out of with an explicit jump.
