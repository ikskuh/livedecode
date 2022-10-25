const std = @import("std");
const ast = @import("ast.zig");

pub var arena: std.heap.ArenaAllocator = undefined;
pub const allo: std.mem.Allocator = arena.allocator();

const split_chars = " \r\n\t";

test {
    _ = ast;
}

fn trim(str: []const u8) []const u8 {
    return std.mem.trim(u8, str, split_chars);
}

var bout: std.io.BufferedWriter(4096, std.fs.File.Writer) = undefined;

fn write(comptime fmt: []const u8, args: anytype) void {
    bout.writer().print(fmt, args) catch std.os.exit(1);
}

pub fn main() !void {
    arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    bout = std.io.bufferedWriter(std.io.getStdOut().writer());
    defer bout.flush() catch {};

    const argv = try std.process.argsAlloc(allo);
    defer std.process.argsFree(allo, argv);

    if (argv.len != 3) @panic("usage: livedecode <spec> <binfile>");

    // Compile script file, so we don't touch the binary
    // when the code isn't valid anyways.
    var script = ast.loadFile(argv[1]);

    var binfile = try std.fs.cwd().openFile(argv[2], .{});
    defer binfile.close();

    var state = State{
        .script = script,
        .file = binfile,
        .fields = std.StringHashMap(Value).init(allo),
        .bitread = null,
    };

    try state.fields.put("file.path", Value{ .str = argv[2] });
    try state.fields.put("file.name", Value{ .str = std.fs.path.basename(argv[2]) });
    try state.fields.put("file.size", Value{ .u64 = try state.file.getEndPos() });

    try state.execute(script.top_level);
}

fn fetchType(state: *State, val_type: Type, length: usize) !Value {
    const reader = state.file.reader();
    const value: Value = switch (val_type) {
        .u8 => Value{ .u8 = try reader.readInt(u8, state.endianess) },
        .u16 => Value{ .u16 = try reader.readInt(u16, state.endianess) },
        .u32 => Value{ .u32 = try reader.readInt(u32, state.endianess) },
        .u64 => Value{ .u64 = try reader.readInt(u64, state.endianess) },
        .i8 => Value{ .i8 = try reader.readInt(i8, state.endianess) },
        .i16 => Value{ .i16 = try reader.readInt(i16, state.endianess) },
        .i32 => Value{ .i32 = try reader.readInt(i32, state.endianess) },
        .i64 => Value{ .i64 = try reader.readInt(i64, state.endianess) },
        .f32 => Value{ .f32 = @bitCast(f32, try reader.readInt(u32, state.endianess)) },
        .f64 => Value{ .f64 = @bitCast(f64, try reader.readInt(u64, state.endianess)) },
        .str => blk: {
            var mem = try allo.alloc(u8, length);
            try reader.readNoEof(mem);
            break :blk Value{ .str = mem };
        },
        .blob => blk: {
            var mem = try allo.alloc(u8, length);
            try reader.readNoEof(mem);
            break :blk Value{ .blob = mem };
        },
        .bitblob => blk: {
            if (state.bitread) |*br| {
                var mem = try allo.alloc(u1, length);
                switch (state.endianess) {
                    .Little => try readBits(&br.le, mem),
                    .Big => try readBits(&br.be, mem),
                }
                break :blk Value{ .bitblob = mem };
            } else {
                @panic("attempt to read bitblob in byteread mode");
            }
        },
        .bits => blk: {
            if (state.bitread) |*br| {
                if (length > 64) @panic("cannot read over 64 bits into an integer");
                break :blk Value{ .bits = switch (state.endianess) {
                    .Little => try br.le.readBitsNoEof(u64, length),
                    .Big => try br.be.readBitsNoEof(u64, length),
                } };
            } else {
                @panic("attempt to read bits in byteread mode");
            }
        },
        .tuple => unreachable,
    };

    return value;
}

const Args = struct {
    state: *State,
    list: []const ast.ValueToken,
    index: usize,

    pub fn init(state: *State, list: []const ast.ValueToken) Args {
        return Args{ .state = state, .list = list, .index = 0 };
    }

    pub fn hasMore(args: Args) bool {
        return (args.index < args.list.len);
    }

    pub fn next(args: *Args) ?ast.ValueToken {
        if (args.index >= args.list.len)
            return null;
        const current = args.list[args.index];
        args.index += 1;
        return current;
    }

    pub fn getIdentifier(args: *Args) []const u8 {
        const tag = args.next() orelse @panic("not enough arguments!");
        if (tag != .identifier)
            std.debug.panic("Expected identifier, got {s}!", .{@tagName(tag)});
        return tag.identifier;
    }

    pub fn getValue(args: *Args) !Value {
        const tag = args.next() orelse @panic("not enough arguments!");

        return try args.state.decode(tag);
    }

    pub fn getInt(args: *Args) u64 {
        const tag = args.next() orelse @panic("not enough arguments!");
        return args.state.decodeInt(tag) catch @panic("expected integer");
    }

    pub fn getString(args: *Args) []const u8 {
        const tag = args.next() orelse @panic("not enough arguments!");

        var val = args.state.decode(tag) catch std.debug.panic("expected string value, got illegal {s}", .{@tagName(tag)});

        return switch (val) {
            .str => |s| s,
            else => std.debug.panic("expected string value, got {s}", .{@tagName(val)}),
        };
    }

    pub fn getTuple(args: *Args) []const Value {
        const tag = args.next() orelse @panic("not enough arguments, expected tuple.");
        if (tag != .tuple) std.debug.panic("expected tuple, got {s}", .{@tagName(tag)});
        return (args.state.decode(tag) catch unreachable).tuple;
    }
};

const Macros = struct {
    pub fn @"if"(state: *State, iter: *Args) !void {
        const condition = try state.decode(iter.next().?);
        const is_met = if (iter.next()) |eql_str| blk: {
            const value = try state.decode(eql_str);

            break :blk (condition.getInt() == value.getInt());
        } else condition.getInt() != 0;
        try state.condstack.append(is_met);
    }

    // pub fn @"elseif"(state: *State, iter: *Args) !void {
    //     try @"endif"(state, iter);
    //     try @"if"(state, iter);
    // }

    pub fn @"else"(state: *State, iter: *Args) !void {
        _ = iter;
        const last = &state.condstack.items[state.condstack.items.len - 1];
        last.* = !last.*;
    }

    pub fn @"endif"(state: *State, iter: *Args) !void {
        _ = iter;
        _ = state.condstack.pop();
    }

    // .loop <count>
    // .loop <count> <var>
    pub fn @"loop"(state: *State, iter: *Args) !void {
        var lop = State.Loop{
            .start_offset = try state.code.getPos(),
            .count = 0,
            .limit = try state.decodeInt(iter.next().?),
            .loopvar = try allo.dupe(u8, iter.next() orelse ""),
        };
        try state.repeatstack.append(lop);
        if (lop.loopvar.len > 0) {
            try state.fields.put(lop.loopvar, Value{ .u64 = lop.count });
        }
    }

    pub fn @"endloop"(state: *State, iter: *Args) !void {
        _ = iter;
        const lop = &state.repeatstack.items[state.repeatstack.items.len - 1];

        if (lop.count == lop.limit) {
            _ = state.repeatstack.pop();
        } else {
            lop.count += 1;
            if (lop.loopvar.len > 0) {
                try state.fields.put(lop.loopvar, Value{ .u64 = lop.count });
            }
            try state.code.seekTo(lop.start_offset);
        }
    }
};

const Commands = struct {
    pub fn def(state: *State, iter: *Args) !void {
        const name = iter.getIdentifier();
        const value = try iter.getValue();
        state.setVariable(name, value);
    }

    pub fn print(state: *State, iter: *Args) !void {
        var suppress_space = false;

        while (iter.next()) |item| {
            if (item == .identifier and std.mem.eql(u8, item.identifier, ";")) {
                suppress_space = true;
            } else if (item == .identifier) {
                write("{s}", .{item.identifier});
                if (!suppress_space) {
                    write(" ", .{});
                }
                suppress_space = false;
            } else {
                var val = state.decode(item) catch |err| Value{ .str = @errorName(err) };
                write("{}", .{val});
                if (!suppress_space) {
                    write(" ", .{});
                }
                suppress_space = false;
            }
        }

        if (!suppress_space) {
            write("\n", .{});
        }
    }

    pub fn seek(state: *State, iter: *Args) !void {
        if (state.bitread != null) @panic("cannot seek while reading bits");
        var offset: u64 = 0;
        var good = false;
        while (iter.next()) |item| {
            good = true;
            offset += try state.decodeInt(item);
        }
        if (!good)
            return;
        try state.file.seekTo(offset);
    }

    pub fn move(state: *State, iter: *Args) !void {
        if (state.bitread != null) @panic("cannot move while reading bits");
        var offset: i64 = 0;
        var good = false;
        while (iter.next()) |item| {
            good = true;
            offset += try state.decodeSignedInt(item);
        }
        if (!good)
            return;
        try state.file.seekBy(offset);
    }

    pub fn tell(state: *State, iter: *Args) !void {
        const where = try state.file.getPos();
        if (iter.hasMore()) {
            state.setVariable(iter.getIdentifier(), .{ .u64 = where });
        }
        write("current file position: {}\n", .{where});
    }

    pub fn dump(state: *State, iter: *Args) !void {
        const len = try state.decodeInt(iter.next().?);

        var line_buffer: [16]u8 = undefined;

        var i: u64 = 0;
        var where = try state.file.getPos();

        while (i < len) : (i += line_buffer.len) {
            const bytes = std.math.min(line_buffer.len, len - i);
            const actual = try state.file.read(line_buffer[0..bytes]);

            write("0x{X:0>8}:", .{where});

            for (line_buffer[0..actual]) |c, j| {
                const col = j;
                if (col % 4 == 0 and col > 0) write(" ", .{});
                write(" {X:0>2}", .{c});
            }
            for (line_buffer[actual..]) |_, j| {
                const col = actual + j;
                if (col % 4 == 0 and col > 0) write(" ", .{});
                write(" __", .{});
            }

            write(" |", .{});

            for (line_buffer[0..actual]) |c| {
                write("{c}", .{
                    if (std.ascii.isPrint(c)) c else '.',
                });
            }
            for (line_buffer[actual..]) |_| {
                write(" ", .{});
            }

            write("|\n", .{});

            where += actual;
            if (actual < bytes)
                break;
        }
    }

    pub fn diskdump(state: *State, iter: *Args) !void {
        const len = iter.getInt();
        const filename = iter.getString();

        var out = std.fs.cwd().createFile(filename, .{}) catch @panic("i/o error");
        defer out.close();

        var i: u64 = 0;
        while (i < len) {
            var buffer: [8192]u8 = undefined;

            const bytes = try state.file.read(&buffer);
            if (bytes == 0) @panic("not enough data");

            out.writer().writeAll(buffer[0..bytes]) catch @panic("i/o error");
        }
    }

    // program <name> <arg0> <arg1>
    pub fn call(state: *State, iter: *Args) !void {
        const pgm_name = iter.getIdentifier();

        const program = for (state.script.programs) |prg| {
            if (std.mem.eql(u8, prg.name, pgm_name))
                break prg;
        } else std.debug.panic("program {s} not found!", .{pgm_name});

        var argc: usize = 0;
        while (iter.next()) |argv| : (argc += 1) {
            const name = try std.fmt.allocPrint(allo, "arg[{}]", .{argc});
            try state.fields.put(name, try state.decode(argv));
        }

        try state.execute(program.code);
    }

    pub fn endian(state: *State, iter: *Args) !void {
        if (state.bitread != null) {
            // there isn't really a logical way to do this since the endian
            // affects the order in which the bitreader goes through bytes
            @panic("cannot switch endianness while reading bits");
        }

        const kind = iter.getIdentifier();

        if (std.ascii.eqlIgnoreCase(kind, "little") or std.ascii.eqlIgnoreCase(kind, "le")) {
            state.endianess = .Little;
        } else if (std.ascii.eqlIgnoreCase(kind, "big") or std.ascii.eqlIgnoreCase(kind, "be")) {
            state.endianess = .Big;
        } else {
            std.debug.panic("invalid endianess: {s}", .{kind});
        }
    }

    pub fn bitread(state: *State, iter: *Args) !void {
        _ = iter;
        state.bitread = switch (state.endianess) {
            .Little => .{ .le = std.io.bitReader(.Little, state.file.reader()) },
            .Big => .{ .be = std.io.bitReader(.Big, state.file.reader()) },
        };
    }

    pub fn byteread(state: *State, iter: *Args) !void {
        _ = iter;
        state.bitread = null;
    }

    pub fn bitmap(state: *State, iter: *Args) !void {
        const PixelFormat = enum {
            rgb565,
            rgb888,
            bgr888,
            rgbx8888,
            rgba8888,
            pub fn bpp(fmt: @This()) usize {
                return switch (fmt) {
                    .rgb565 => 2,
                    .rgb888 => 3,
                    .bgr888 => 3,
                    .rgbx8888 => 4,
                    .rgba8888 => 4,
                };
            }
        };

        const width = iter.getInt();
        const height = iter.getInt();
        const format = std.meta.stringToEnum(PixelFormat, iter.getIdentifier()) orelse @panic("invalid bitmap format");

        const writeout = if (iter.next()) |tag|
            (try state.decode(tag)).str
        else
            "";

        const buffer_size = width * height * format.bpp();

        if (writeout.len > 0) {
            var out = std.fs.cwd().createFile(writeout, .{}) catch @panic("i/o error");
            defer out.close();

            out.writer().print("P6 {} {} 255\n", .{ width, height }) catch @panic("i/o error");

            switch (format) {
                .rgb565 => {
                    var i: usize = 0;
                    while (i < buffer_size) {
                        var copy: [2]u8 = undefined;
                        state.file.reader().readNoEof(&copy) catch @panic("i/o error");

                        const Rgb = packed struct {
                            r: u5,
                            g: u6,
                            b: u5,
                        };

                        const rgb = @bitCast(Rgb, copy);

                        var vals = [3]u8{
                            @as(u8, rgb.b) << 3 | @as(u8, rgb.b) >> 2,
                            @as(u8, rgb.g) << 2 | @as(u8, rgb.g) >> 4,
                            @as(u8, rgb.r) << 3 | @as(u8, rgb.r) >> 2,
                        };

                        out.writeAll(&vals) catch @panic("i/o error");

                        i += copy.len;
                    }
                },
                .rgb888 => {
                    var i: usize = 0;
                    while (i < buffer_size) {
                        var copy: [8192]u8 = undefined;
                        const maxlen = std.math.min(copy.len, buffer_size - i);

                        const len = state.file.read(copy[0..maxlen]) catch @panic("i/o error");
                        if (len == 0) @panic("unexpected eof in bitmap");

                        out.writeAll(copy[0..len]) catch @panic("i/o error");

                        i += copy.len;
                    }
                },
                .bgr888 => {
                    var i: usize = 0;
                    while (i < buffer_size) {
                        var copy: [3]u8 = undefined;
                        state.file.reader().readNoEof(&copy) catch @panic("i/o error");

                        std.mem.swap(u8, &copy[0], &copy[2]);

                        out.writeAll(&copy) catch @panic("i/o error");

                        i += copy.len;
                    }
                },
                .rgbx8888 => @panic("rgbx8888 not supported for writeout yet."),
                .rgba8888 => @panic("rgba8888 not supported for writeout yet."),
            }
        } else {
            try state.file.seekBy(@intCast(i64, buffer_size));
        }
    }

    pub fn lut(state: *State, iter: *Args) !void {
        const value = try state.decodeInt(iter.next().?);

        // TODO: Port to tuples
        while (iter.next()) |kv_src| {
            if (kv_src != .tuple) @panic("lut expects a list of tuples!");

            const kv = (state.decode(kv_src) catch unreachable).tuple;

            if (kv.len != 2) @panic("lut expects 2-tuples.");

            const key = kv[0].getInt();
            const tag = kv[1].str;

            if (value == key) {
                write("{} => {s}\n", .{ key, tag });
            }
        }
    }

    pub fn divs(state: *State, iter: *Args) !void {
        const value = try state.decodeInt(iter.next().?);

        write("{} is divided by ", .{value});

        var i: u64 = value;
        var first = true;
        while (i > 0) : (i -= 1) {
            if ((value % i) == 0) {
                if (!first) write(", ", .{});
                write("{}", .{i});
                first = false;
            }
        }
        write("\n", .{});
    }

    pub fn findpattern(state: *State, iter: *Args) !void {
        const MatchSet = std.bit_set.ArrayBitSet(usize, 256);

        var list = std.ArrayList(MatchSet).init(allo);
        defer list.deinit();

        while (iter.next()) |item| {
            if (item == .identifier and std.mem.eql(u8, item.identifier, "*")) {
                try list.append(MatchSet.initFull());
            } else {
                var set = MatchSet.initEmpty();
                var opts = std.mem.tokenize(u8, item.identifier, "|");
                while (opts.next()) |key| {
                    const index = try std.fmt.parseInt(u8, key, 0);
                    set.set(index);
                }
                try list.append(set);
            }
        }

        var buffer = try allo.alloc(u8, list.items.len);
        defer allo.free(buffer);

        const H = struct {
            fn isMatch(pattern: []const MatchSet, seq: []const u8) bool {
                std.debug.assert(pattern.len == seq.len);
                for (pattern) |mc, i| {
                    if (!mc.isSet(seq[i]))
                        return false;
                }
                return true;
            }
        };

        try state.file.reader().readNoEof(buffer);

        while (true) {
            if (H.isMatch(list.items, buffer)) {
                const offset = try state.file.getPos();
                write("found match at {}: {any}\n", .{ offset - buffer.len, buffer });
            }

            std.mem.copy(u8, buffer[0..], buffer[1..]);
            buffer[buffer.len - 1] = try state.file.reader().readByte();
        }
    }

    // array <type> <len>? <pattern> <length>
    pub fn array(state: *State, iter: *Args) !void {
        const type_name = iter.getIdentifier();
        const val_type = std.meta.stringToEnum(Type, type_name) orelse std.debug.panic("unknown type: {s}", .{type_name});
        const val_length = if (val_type.requiresLength())
            try state.decodeInt(iter.next().?)
        else
            0;

        const name_pattern = iter.getIdentifier();
        const name_pattern_split = std.mem.indexOfScalar(u8, name_pattern, '?') orelse @panic("name requires ? placeholder for index");

        const length = try state.decodeInt(iter.next().?);

        var i: usize = 0;
        while (i < length) : (i += 1) {
            var index_buf: [32]u8 = undefined;
            const index = std.fmt.bufPrint(&index_buf, "{d}", .{i}) catch unreachable;

            const value = try fetchType(state, val_type, val_length);

            const name = try std.mem.join(allo, "", &.{
                name_pattern[0..name_pattern_split],
                index,
                name_pattern[name_pattern_split + 1 ..],
            });

            write("{s: >20} = {}\n", .{ name, value });
            try state.fields.put(name, value);
        }
    }

    // template <variable> <pattern> <item> <item> <item>
    pub fn select(state: *State, iter: *Args) !void {
        const target_var = iter.getIdentifier();
        const pattern = iter.getIdentifier();

        var src_name = std.ArrayList(u8).init(allo);

        var pos: usize = 0;
        while (std.mem.indexOfScalarPos(u8, pattern, pos, '?')) |item| {
            defer pos = item + 1;

            const slice = pattern[pos..item];
            try src_name.appendSlice(slice);

            const value = try state.decode(iter.next().?);

            try src_name.writer().print("{}", .{value});
        }

        try src_name.appendSlice(pattern[pos..]);

        const value = state.fields.get(src_name.items) orelse std.debug.panic("Variable {s} not found!", .{src_name.items});
        try state.fields.put(try allo.dupe(u8, target_var), value);
    }
};

const State = struct {
    // io:
    file: std.fs.File,
    endianess: std.builtin.Endian = .Little,
    bitread: ?union { // non-null while in bitread mode
        le: std.io.BitReader(.Little, std.fs.File.Reader),
        be: std.io.BitReader(.Big, std.fs.File.Reader),
    },

    // runtime:
    script: ast.Script,
    fields: std.StringHashMap(Value),

    fn decodeInt(state: State, tag: ast.ValueToken) !u64 {
        var val = try state.decode(tag);
        return val.getInt();
    }

    fn decodeSignedInt(state: State, tag: ast.ValueToken) !i64 {
        var val = try state.decode(tag);
        return val.getSignedInt();
    }

    fn decode(state: State, tag: ast.ValueToken) !Value {
        return switch (tag) {
            .variable_ref => |name| state.fields.get(name) orelse std.debug.panic("var {s} not found", .{name}),
            .string => |str| Value{ .str = str },
            .identifier => |id| Value{ .str = id },
            .number => |str| if (std.fmt.parseInt(i64, str, 0)) |sval|
                Value{ .i64 = sval }
            else |_| if (std.fmt.parseInt(u64, str, 0)) |ival|
                Value{ .u64 = ival }
            else |_|
                Value{ .f32 = try std.fmt.parseFloat(f32, str) },
            .tuple => |src| blk: {
                const tup = try allo.alloc(Value, src.len);
                for (tup) |*dst, i| {
                    dst.* = try state.decode(src[i]);
                }
                break :blk Value{ .tuple = tup };
            },
        };
    }

    pub fn read(state: *State, buf: []u8) !usize {
        return if (state.bitread) |*br| switch (state.endianess) {
            .Little => br.le.reader().read(buf),
            .Big => br.be.reader().read(buf),
        } else state.file.reader().read(buf);
    }

    fn reader(state: *State) std.io.Reader(*State, std.fs.File.Reader.Error, read) {
        return .{ .context = state };
    }

    pub fn setVariable(state: *State, name: []const u8, value: Value) void {
        const gop = state.fields.getOrPut(name) catch @panic("oom");
        if (!gop.found_existing) {
            gop.key_ptr.* = allo.dupe(u8, name) catch @panic("oom");
        }
        gop.value_ptr.* = value;
    }

    const ExecError = error{ OutOfMemory, InvalidCharacter, Overflow, EndOfStream } || std.fs.File.ReadError || std.fs.File.SeekError;
    pub fn execute(state: *State, seq: ast.Sequence) ExecError!void {
        for (seq.instructions) |instr| {
            try state.executeOne(instr);
        }
    }

    pub fn executeOne(state: *State, instr: ast.Node) ExecError!void {
        switch (instr) {
            .command => |info| {
                var cmditer = Args.init(state, info.arguments);

                inline for (@typeInfo(Commands).Struct.decls) |decl| {
                    if (std.mem.eql(u8, decl.name, info.name)) {
                        try @field(Commands, decl.name)(state, &cmditer);
                        return;
                    }
                }

                // type name
                const val_type = std.meta.stringToEnum(Type, info.name) orelse std.debug.panic("unknown type: {s}", .{info.name});

                const length = if (val_type.requiresLength())
                    try state.decodeInt(cmditer.next().?)
                else
                    0;

                const value = try fetchType(state, val_type, length);

                if (cmditer.next()) |name| {
                    write("{s: >20} = {}\n", .{ name.identifier, value });
                    state.setVariable(name.identifier, value);
                }
            },
            .decision => |info| {
                const value = try state.decodeInt(info.value);

                const equals = if (info.comparison) |comp|
                    value == try state.decodeInt(comp)
                else
                    value != 0;

                try state.execute(if (equals)
                    info.true_body
                else
                    info.false_body orelse return);
            },
            .loop => |info| {
                const limit = try state.decodeInt(info.count);

                var count: u64 = 0;
                while (count < limit) : (count += 1) {
                    if (info.variable) |varname|
                        state.setVariable(varname, Value{ .u64 = count });
                    try state.execute(info.body);
                }
            },
            .breakloop => @panic("not implemented yet"),
        }
    }
};

const Value = union(Type) {
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
    str: []const u8,
    blob: []const u8,
    tuple: []Value,

    // bitread cases
    bitblob: []const u1, // this is really inefficient but it's fine
    bits: u64,

    pub fn getInt(val: Value) u64 {
        return switch (val) {
            .u8 => |v| v,
            .u16 => |v| v,
            .u32 => |v| v,
            .u64 => |v| v,
            .i8 => |v| @bitCast(u8, v),
            .i16 => |v| @bitCast(u16, v),
            .i32 => |v| @bitCast(u32, v),
            .i64 => |v| @bitCast(u64, v),
            .f32 => |v| @floatToInt(u64, v),
            .f64 => |v| @floatToInt(u64, v),
            .str => |v| std.fmt.parseInt(u64, v, 0) catch unreachable,
            .blob => @panic("blob is not an int"),
            .bitblob => @panic("bitblob is not an int"),
            .bits => |v| v,
            .tuple => @panic("tuple is not convertible to int."),
        };
    }

    pub fn getSignedInt(val: Value) i64 {
        return switch (val) {
            .u8 => |v| @bitCast(i8, v),
            .u16 => |v| @bitCast(i16, v),
            .u32 => |v| @bitCast(i32, v),
            .u64 => |v| @bitCast(i64, v),
            .i8 => |v| v,
            .i16 => |v| v,
            .i32 => |v| v,
            .i64 => |v| v,
            .f32 => |v| @floatToInt(i64, v),
            .f64 => |v| @floatToInt(i64, v),
            .str => |v| std.fmt.parseInt(i64, v, 0) catch unreachable,
            .blob => @panic("blob is not an int"),
            .bitblob => @panic("bitblob is not an int"),
            .bits => |v| @intCast(i64, v), // TODO we might want to store the number of bits used so we can convert to signed
            .tuple => @panic("tuple is not convertible to int."),
        };
    }

    pub fn format(val: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (val) {
            .str => |str| try writer.print("\"{}\"", .{std.zig.fmtEscapes(str)}),
            .blob => |str| try writer.print("{any}", .{str}),
            .tuple => |tup| {
                try writer.writeAll("(");
                for (tup) |item, i| {
                    if (i > 0)
                        try writer.writeAll(" ");
                    try writer.print("{}", .{item});
                }
                try writer.writeAll(")");
            },
            inline else => |v| try writer.print("{d}", .{v}),
        }
    }
};

fn readBits(br: anytype, out: []u1) !void {
    for (out) |*b| {
        b.* = try br.readBitsNoEof(u1, 1);
    }
}

const Type = enum {
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,
    str,
    blob,
    bitblob,
    bits,
    tuple,

    pub fn requiresLength(t: Type) bool {
        return switch (t) {
            .u8 => false,
            .u16 => false,
            .u32 => false,
            .u64 => false,
            .i8 => false,
            .i16 => false,
            .i32 => false,
            .i64 => false,
            .f32 => false,
            .f64 => false,
            .str => true,
            .blob => true,
            .bitblob => true,
            .bits => true,
            .tuple => @panic("tuples cannot be read from the stream"),
        };
    }
};
