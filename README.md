# livedecode

Decode binary data on the fly quicker.

## Usage

```sh-session
[user@host project]$ livedecode example/png.spec example/example.png
```

The tool is meant to be run live while typing on a spec, so one can do stuff like this:
![screenshot of program usage](doc/screenshot.png)

Just run the program periodically in the background:

```sh-session
[user@host project]$ while true; do
  clear
  date
  livedecode docs/wmb6.spec data/wmb/wmb6/block.wmb > /tmp/dump.txt
  sleep 1
done
```

## Syntax

The format is a very crude line based syntax. Empty lines are ignored, everything past a `#` is a comment.
Lines are split into tokens separated by either space or tab characters.

The first token in a line determines the command or type of the line.
A line starting with `.` is a macro and is always executed. All other lines can be conditionally be executed.

If a line starts with a type, this type is decoded. If a name is given after the type, a variable is created with that name.

In a lot of places where not a name is expected, either immediate numbers can be written as decimal, hexadecimal (`0x1A`) or a
variable reference can be used (`*variable`).

## Commands, Macros and Variables

### Commands

`print …`

prints all arguments. if an argument is a semicolon, no space is printed after the argument. If a semicolon is last, no line feed is printed.

#### `seek <offset> …`

sums up all offsets and moves the read cursor to the absolute position

#### `move <offset> …`

sums up all offsets and moves the read cursor relatively. Accepts negative numbers

#### `tell`

prints the file cursor

#### `tell <variable`

stores the file cursor in <variable>

#### `dump <len>`

dumps <len> bytes

#### `pgm <name>`

creates a new program called <name>

#### `! …`

appends everything past the ! to the last created program

#### `replay <pgm> …`

invokes a program named <pgm>. all arguments past that are passed as variables arg[0] to arg[n]

#### `array <type> <pattern> <length>`

Creates an array of <length> items called <pattern>. In <pattern>, the first occurance of `?` will be replaced with the array index. <type> determines the type of the array items.

#### `array <type> <len> <pattern> <length>`

Creates an array of <length> items called <pattern> where <type> is a sized type (str, blob, ...). In <pattern>, the first occurance of `?` will be replaced with the array index. <type> determines the type of the array items.

#### `select <var> <pattern> <key> …`

Builds a variable name from <pattern> and all provided <key>s. For each key, the next `?` in the <pattern> is replaced with the value of <key>.
After all `?` are resolved, a global lookup is performed and the variable with the computed name is then copied into <var>.

#### `endian le`

changes integer endianess to little endian

#### `endian little`

changes integer endianess to little endian

#### `endian be`

changes integer endianess to big endian

#### `endian big`

changes integer endianess to big endian

#### `bitread`

switches to bit-reading mode

#### `byteread`

switches out of bit-reading mode, discarding any unread bits in the current byte

#### `bitmap <width> <height> <format`

consumes a bitmap of size <width>\*<height> and <format> rgb565, rgb888, bgr888, rgbx8888 or rgba8888

#### `bitmap <width> <height> <format> <writeout`

same as previous, but saves result to a PPM file called <writeout>

#### `lut <index> <key> <tag> <key> <tag`

Will perform a lookup on value <index>. If <index> matches <key>, the following <tag> is printed. Any number of <key> <tag> pairs can be passed.

#### `divs <value`

prints all possible integer divisors of <value>

#### `diskdump <len> <filename`

Reads <len> bytes and writes them into a file called <filename>. Useful to extract portions of a file.

#### `findpattern …`

All arguments together form a pattern. This pattern is then searched in the file from the cursor position on and each occurrence is printed with offset.

Pattern components can either be a `*` for any kind of byte, or a list of `|`-separated integers that list the possibilities for this option.

To make this more clear, let's consider this example:  
We're searching for a list of u32 that can only consist of the integer values 1, 2 or 3, but there's a unknown length marker at the start that is a 16 bit value less than 256:

```rb
# Search for at least 3 items:
#           len  item 0       item 1       item 2
findpattern * 0  1|2|3 0 0 0  1|2|3 0 0 0  1|2|3 0 0 0
```

### Macros

```rb
.if <value>          # the code following this will be executed if <value> is not 0
.if <value> <equals> # the code following this will be executed if <value> is equals to <equals>
.else                # swaps the current execution condition
.endif               # ends a if block
.def <name> <value>  # creates a variable called <name> with the value <value>. Useful for constants or aliases
.loop <count>        # Repeats the following code for <count> times.
.loop <count> <var>  # Repeats the following code for <count> times. Writes the current index into <var>.
.endloop             # Terminates the current loop
```

### Types

```rb
u8            #  8 bit unsigned integer
u16           # 16 bit unsigned integer
u32           # 32 bit unsigned integer
u64           # 64 bit unsigned integer
i8            #  8 bit signed integer, two's complement
i16           # 16 bit signed integer, two's complement
i32           # 32 bit signed integer, two's complement
i64           # 64 bit signed integer, two's complement
f32           # 32 bit floating point
f64           # 64 bit floating point
str  <len>    # ascii string of <len> bytes, displayed as string+escapes
blob <len>    # binary blob of <len> bytes, displayed as array
bitblob <len> # binary blob of <len> bits, displayed as array of bits (only valid in bit-reading mode)
bits <len>    # an unsigned integer of <len> bits up to 64 (only valid in bit-reading mode)
```

### Predefined variables

```sh
str <?> file.path # The full path of the current file
str <?> file.name # The file name of the current file
u64     file.size # Size of the current file in bytes
```

## Example

```
endian le
u32 magic
u32 type
u32 offset
u32 length

print section 1
seek *offset
dump *length

.if *type 10
  seek 0x200
  str 10 description
.endif
```

A more complete example can be found [here](example/), which will decode a good amount of a PNG file.

## Building

1. Fetch the latest zig install (tested with `0.10.0-dev.4442+ce3ffa5e1`).
2. Invoke `zig build`.
3. Install `zig-out/bin/livedecode` into your system in a way you like it.
