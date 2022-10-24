const std = @import("std");

var arena: std.heap.ArenaAllocator = undefined;
var allo: std.mem.Allocator = arena.allocator();

const split_chars = " \r\n\t";

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

    var binfile = try std.fs.cwd().openFile(argv[2], .{});
    defer binfile.close();

    var specfile = try std.fs.cwd().openFile(argv[1], .{});
    defer specfile.close();

    var specreader = specfile.reader();

    var state = State{
        .file = binfile,
        .code = specfile,
        .fields = std.StringHashMap(Value).init(allo),
        .programs = std.StringHashMap(std.ArrayList([]const u8)).init(allo),
        .condstack = std.ArrayList(bool).init(allo),
        .repeatstack = std.ArrayList(State.Loop).init(allo),
        .bitread = null,
    };
    try state.condstack.append(true);

    try state.fields.put("file.path", Value{ .str = argv[2] });
    try state.fields.put("file.name", Value{ .str = std.fs.path.basename(argv[2]) });
    try state.fields.put("file.size", Value{ .u64 = try state.file.getEndPos() });

    while (true) {
        var cmdbuffer: [1024]u8 = undefined;
        var line = trim((try specreader.readUntilDelimiterOrEof(&cmdbuffer, '\n')) orelse break);

        if (std.mem.indexOfScalar(u8, line, '#')) |index| line = line[0..index];

        if (line.len > 0) {
            exec(line, &state) catch |err| {
                std.debug.print("error in line:\n{s}\n", .{line});
                return err;
            };
        }
    }
}

const ExecError = error{ OutOfMemory, InvalidCharacter, Overflow, EndOfStream } || std.fs.File.ReadError || std.fs.File.SeekError;
fn exec(line: []const u8, state: *State) ExecError!void {
    var cmditer = std.mem.tokenize(u8, line, split_chars);
    const cmd = cmditer.next().?;

    if (std.mem.startsWith(u8, cmd, ".")) {
        inline for (@typeInfo(Macros).Struct.decls) |decl| {
            if (std.mem.eql(u8, decl.name, cmd[1..])) {
                try @field(Macros, decl.name)(state, &cmditer);
                return;
            }
        }
        std.debug.panic("unknown macro {s}", .{cmd});
    }

    // skip all non-macro lines
    if (!state.enabled())
        return;

    inline for (@typeInfo(Commands).Struct.decls) |decl| {
        if (std.mem.eql(u8, decl.name, cmd)) {
            try @field(Commands, decl.name)(state, &cmditer);
            return;
        }
    }

    // type name
    const val_type = std.meta.stringToEnum(Type, cmd) orelse std.debug.panic("unknown type: {s}", .{cmd});

    const length = if (val_type.requiresLength())
        try state.decodeInt(cmditer.next().?)
    else
        0;

    const value = try fetchType(state, val_type, length);

    if (cmditer.next()) |name| {
        write("{s: >20} = {}\n", .{ name, value });
        try state.fields.put(try allo.dupe(u8, name), value);
    }
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
    };

    return value;
}

const Args = std.mem.TokenIterator(u8);

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
        const name = try allo.dupe(u8, iter.next().?);
        const value = try state.decode(iter.next().?);
        try state.fields.put(name, value);
    }

    pub fn print(state: *State, iter: *Args) !void {
        var suppress_space = false;

        while (iter.next()) |item| {
            if (std.mem.eql(u8, item, ";")) {
                suppress_space = true;
            } else {
                var val = state.decode(item) catch Value{ .str = item };
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
        if (iter.next()) |value| {
            try state.fields.put(try allo.dupe(u8, value), .{ .u64 = where });
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
        const len = try state.decodeInt(iter.next().?);
        const filename = iter.next().?;

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

    // pgm name
    pub fn pgm(state: *State, iter: *Args) !void {
        const name = iter.next().?;

        const gop = try state.programs.getOrPut(try allo.dupe(u8, name));
        if (gop.found_existing)
            @panic("program already exists");

        gop.value_ptr.* = std.ArrayList([]const u8).init(allo);
        state.current_pgm = gop.value_ptr;
    }

    // ! line
    pub fn @"!"(state: *State, iter: *Args) !void {
        try state.current_pgm.?.append(try allo.dupe(u8, trim(iter.buffer[1..])));
    }

    // program <name> <arg0> <arg1>
    pub fn call(state: *State, iter: *Args) !void {
        const pgm_name = iter.next().?;

        var argc: usize = 0;
        while (iter.next()) |argv| : (argc += 1) {
            const name = try std.fmt.allocPrint(allo, "arg[{}]", .{argc});
            try state.fields.put(name, try state.decode(argv));
        }

        const code = state.programs.get(pgm_name) orelse @panic("program not found");
        for (code.items) |cmd| {
            try exec(cmd, state);
        }
    }

    pub fn endian(state: *State, iter: *Args) !void {
        if (state.bitread != null) {
            // there isn't really a logical way to do this since the endian
            // affects the order in which the bitreader goes through bytes
            @panic("cannot switch endianness while reading bits");
        }

        const kind = iter.next().?;

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

        const width = try state.decodeInt(iter.next().?);
        const height = try state.decodeInt(iter.next().?);
        const format = std.meta.stringToEnum(PixelFormat, iter.next().?) orelse @panic("invalid bitmap format");

        const writeout = iter.next() orelse "";

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

        while (iter.next()) |key| {
            const comp = try state.decodeInt(key);
            const tag = iter.next().?;

            if (value == comp) {
                write("{} => {s}\n", .{ comp, tag });
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
            if (std.mem.eql(u8, item, "*")) {
                try list.append(MatchSet.initFull());
            } else {
                var set = MatchSet.initEmpty();
                var opts = std.mem.tokenize(u8, item, "|");
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
        const type_name = iter.next().?;
        const val_type = std.meta.stringToEnum(Type, type_name) orelse std.debug.panic("unknown type: {s}", .{type_name});
        const val_length = if (val_type.requiresLength())
            try state.decodeInt(iter.next().?)
        else
            0;

        const name_pattern = iter.next().?;
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
        const target_var = iter.next().?;
        const pattern = iter.next().?;

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
    endianess: std.builtin.Endian = .Little,
    file: std.fs.File,
    code: std.fs.File,
    fields: std.StringHashMap(Value),
    condstack: std.ArrayList(bool),
    bitread: ?union { // non-null while in bitread mode
        le: std.io.BitReader(.Little, std.fs.File.Reader),
        be: std.io.BitReader(.Big, std.fs.File.Reader),
    },
    repeatstack: std.ArrayList(Loop),

    fn enabled(state: State) bool {
        for (state.repeatstack.items) |loop| {
            if (loop.count >= loop.limit)
                return false;
        }
        return std.mem.allEqual(bool, state.condstack.items, true);
    }

    fn decodeInt(state: State, str: []const u8) !u64 {
        var val = try state.decode(str);
        return val.getInt();
    }

    fn decodeSignedInt(state: State, str: []const u8) !i64 {
        var val = try state.decode(str);
        return val.getSignedInt();
    }

    fn decode(state: State, str: []const u8) !Value {
        return if (std.mem.startsWith(u8, str, "*"))
            state.fields.get(str[1..]) orelse std.debug.panic("var {s} not found", .{str[1..]})
        else if (std.mem.startsWith(u8, str, "\""))
            Value{ .str = try allo.dupe(u8, str[1 .. str.len - 1]) }
        else if (std.fmt.parseInt(i64, str, 0)) |sval|
            Value{ .i64 = sval }
        else |_|
            Value{ .u64 = try std.fmt.parseInt(u64, str, 0) };
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

    const Loop = struct {
        start_offset: u64,
        count: u64,
        limit: u64,
        loopvar: []const u8,
    };
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
        };
    }

    pub fn format(val: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (val) {
            .str => |str| try writer.print("'{}'", .{std.fmt.fmtSliceEscapeUpper(str)}),
            .blob => |str| try writer.print("{any}", .{str}),
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
        };
    }
};

const Ast = struct {
    pub const Script = struct {
        top_level: Sequence,
        programs: []Program,
    };

    pub const Sequence = struct {
        is_top_level: bool,
        instructions: []Node,
    };

    pub const Node = union(enum) {
        decision: Conditional,
        loop: Loop,
        breakloop,
        command: Command,
        program: Program, // only legal on top-level sequence
    };

    pub const Program = struct {
        name: []const u8,
        code: Sequence,
    };

    pub const Conditional = struct {
        value: ValueToken,
        comparison: ?ValueToken,

        true_body: Sequence,
        false_body: ?Sequence,
    };

    pub const Loop = struct {
        count: ValueToken,
        variable: ?[]const u8,

        body: Sequence,
    };

    pub const Command = struct {
        name: []const u8,
        arguments: []ValueToken,
    };

    pub const ValueToken = union(enum) {
        variable_ref: []const u8, // *foo
        string: []const u8, // "hello, world"-
        tuple: []ValueToken, // ( <token> <token> <token> )
        identifier: []const u8, // the rest: 10, hello, 24.5, foo_bar
    };

    const ptk = @import("parser-toolkit");

    const TokenType = enum {
        macro,
        identifier,
        @"(",
        @")",
        star_ref,
        string,
        line_feed,
        whitespace,
        comment,
    };

    const Pattern = ptk.Pattern(TokenType);
    const Token = Tokenizer.Token;

    const whitespace_chars = " \t";

    const identifier_matcher = ptk.matchers.takeNoneOf(whitespace_chars ++ ")(\r\n");

    const Tokenizer = ptk.Tokenizer(TokenType, &.{
        Pattern.create(.line_feed, ptk.matchers.linefeed),
        Pattern.create(.comment, ptk.matchers.sequenceOf(.{ ptk.matchers.literal("#"), ptk.matchers.takeNoneOf("\n") })),
        Pattern.create(.@"(", ptk.matchers.literal("(")),
        Pattern.create(.@")", ptk.matchers.literal(")")),
        Pattern.create(.string, matchString),
        Pattern.create(.macro, ptk.matchers.sequenceOf(.{ ptk.matchers.literal("."), identifier_matcher })),
        Pattern.create(.star_ref, ptk.matchers.sequenceOf(.{ ptk.matchers.literal("*"), identifier_matcher })),
        Pattern.create(.identifier, identifier_matcher),
        Pattern.create(.whitespace, ptk.matchers.takeAnyOf(whitespace_chars)),
    });

    fn matchString(string: []const u8) ?usize {
        if (string.len < 2) return null;
        if (string[0] != '"') return null;

        var i: usize = 1;
        while (i < string.len) {
            if (string[i] == '"') {
                return i + 1;
            }
            if (string[i] == '\\') {
                if (i + 1 == string.len) return null;
                i += 1;
            }
            i += 1;
        }

        return null;
    }

    pub fn loadFile(file_name: []const u8) Script {
        const source_code = std.fs.cwd().readFileAlloc(allo, 1 << 20) catch @panic("oom");

        return load(source_code, file_name);
    }

    pub fn load(source_code: []const u8, file_name: ?[]const u8) Script {
        var tokenizer = Tokenizer.init(source_code, file_name);

        // Script

        var script = Script{
            .programs = allo.alloc(Program, 0) catch @panic("oom"),
            .top_level = undefined,
        };
        script.top_level = parseSequence(&script, &tokenizer, null);
        return script;
    }

    fn next(stream: *Tokenizer) ?Token {
        while (true) {
            var t = (stream.next() catch std.debug.panic("invalid token: '{}'", .{std.zig.fmtEscapes(stream.source[stream.offset..])})) orelse return null;

            switch (t.type) {
                .whitespace, .comment => {},
                else => return t,
            }
        }
    }

    fn isStr(a: Token, b: []const u8) bool {
        return std.mem.eql(u8, a.text, b);
    }

    fn fetchId(stream: *Tokenizer) []const u8 {
        const id = next(stream) orelse @panic("identifier expected");
        if (id.type != .identifier) @panic("identifier expected");
        return id.text;
    }

    fn endOfLine(stream: *Tokenizer) void {
        const id = next(stream) orelse return;
        if (id.type != .line_feed)
            @panic("Expected end of line!");
    }

    const Macro = enum {
        loop,
        endloop,

        pgm,
        endpgm,

        @"if",
        elseif,
        @"else",
        endif,
    };

    fn makeSingleTerminator(comptime mac: Macro) fn (Macro) bool {
        return struct {
            fn f(t: Macro) bool {
                return t == mac;
            }
        }.f;
    }

    var if_terminator: Macro = undefined;
    fn isEndOrElseOrElseIf(m: Macro) bool {
        if_terminator = m;
        return m == .endif or m == .elseif or m == .@"else";
    }

    fn parseSequence(script: *Script, stream: *Tokenizer, terminator: ?*const fn (Macro) bool) Sequence {
        var list = std.ArrayList(Node).init(allo);
        defer list.deinit();

        while (true) {
            const first_token: Token = next(stream) orelse if (terminator == null)
                return Sequence{ .is_top_level = true, .instructions = list.toOwnedSlice() }
            else
                std.debug.panic("Did not expect end of script.", .{});

            switch (first_token.type) {
                .macro => {
                    const mac = std.meta.stringToEnum(Macro, first_token.text[1..]) orelse std.debug.panic("Unknown macro {s}", .{first_token.text});

                    if (terminator != null and terminator.?(mac)) {
                        return Sequence{
                            .is_top_level = false,
                            .instructions = list.toOwnedSlice(),
                        };
                    }

                    switch (mac) {
                        .@"if" => {
                            while (true) {
                                const value = parseTokenValue(stream, next(stream) orelse @panic("expected condition value"));

                                const maybe_comp = next(stream);

                                const comparison = if (maybe_comp != null and maybe_comp.?.type != .line_feed)
                                    parseTokenValue(stream, maybe_comp.?)
                                else
                                    null;

                                if (comparison != null)
                                    endOfLine(stream); // terminate .if or .elseif

                                const true_block = parseSequence(undefined, stream, isEndOrElseOrElseIf);

                                if (if_terminator == .endif) {
                                    endOfLine(stream); // terminate .endif

                                    list.append(Node{
                                        .decision = Conditional{
                                            .value = value,
                                            .comparison = comparison,
                                            .true_body = true_block,
                                            .false_body = null,
                                        },
                                    }) catch @panic("oom");

                                    break;
                                }
                                if (if_terminator == .@"else") {
                                    const false_block = parseSequence(undefined, stream, isEndOrElseOrElseIf);
                                    endOfLine(stream);

                                    list.append(Node{
                                        .decision = Conditional{
                                            .value = value,
                                            .comparison = comparison,
                                            .true_body = true_block,
                                            .false_body = false_block,
                                        },
                                    }) catch @panic("oom");

                                    break;
                                }
                            }
                        },

                        .loop => {
                            const count = parseTokenValue(stream, next(stream) orelse @panic("expected loop counter!"));

                            const var_or_term = next(stream) orelse Token{ .type = .line_feed, .text = "\n", .location = stream.current_location };
                            const variable = switch (var_or_term.type) {
                                .line_feed => null,
                                .identifier => var_or_term.text,
                                else => @panic("unexpected token"),
                            };
                            if (variable != null)
                                endOfLine(stream);

                            var seq = parseSequence(undefined, stream, makeSingleTerminator(.endloop));

                            endOfLine(stream);

                            list.append(Node{
                                .loop = Loop{
                                    .count = count,
                                    .variable = variable,
                                    .body = seq,
                                },
                            }) catch @panic("oom");
                        },

                        .pgm => {
                            if (terminator != null)
                                @panic("Nested .pgm is not allowed");

                            var pgm = Program{
                                .name = fetchId(stream),
                                .code = undefined,
                            };
                            endOfLine(stream);
                            pgm.code = parseSequence(undefined, stream, makeSingleTerminator(.endpgm));
                            endOfLine(stream); // .endpgm has no possible arg

                            const new = allo.realloc(script.programs, script.programs.len + 1) catch @panic("oom");
                            new[new.len - 1] = pgm;
                            script.programs = new;
                        },

                        .endloop, .endpgm, .endif, .elseif, .@"else" => std.debug.print("Unexpected token {s}", .{first_token.text}),
                    }
                },

                // any command or type decoding
                .identifier => {
                    var cmd = Command{
                        .name = first_token.text,
                        .arguments = parseTokenList(stream, false),
                    };
                    list.append(Node{ .command = cmd }) catch @panic("oom");
                },
                .@"(", .@")", .star_ref, .string => std.debug.panic("illegal start of command: '{s}'", .{first_token.text}),
                .line_feed => continue, // empty line
                .whitespace, .comment => unreachable,
            }
        }
    }

    fn parseTokenValue(stream: *Tokenizer, item: Token) ValueToken {
        return switch (item.type) {
            .macro, .identifier => ValueToken{ .identifier = item.text },
            .star_ref => ValueToken{ .variable_ref = item.text[1..] },
            .string => ValueToken{ .string = unescapeString(item.text[1 .. item.text.len - 1]) },
            .@"(" => ValueToken{ .tuple = parseTokenList(stream, true) },
            .@")", .line_feed => std.debug.panic("Expected value, got {s}", .{@tagName(item.type)}),
            .whitespace, .comment => unreachable,
        };
    }

    fn parseTokenList(stream: *Tokenizer, is_tuple: bool) []ValueToken {
        var items = std.ArrayList(ValueToken).init(allo);
        defer items.deinit();

        while (true) {
            const item = next(stream) orelse if (is_tuple) @panic("tuple declaration is not closed") else return items.toOwnedSlice();

            const val = switch (item.type) {
                .@")" => if (is_tuple)
                    return items.toOwnedSlice()
                else
                    @panic("no tuple declaration found"),
                .line_feed => if (is_tuple)
                    @panic("tuple declaration is not closed")
                else
                    return items.toOwnedSlice(),

                else => parseTokenValue(stream, item),
            };

            items.append(val) catch @panic("oom");
        }
    }

    fn unescapeString(str: []const u8) []const u8 {
        // TODO: Implement this!
        return str;
    }

    fn runTest(source: []const u8) Script {
        arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        return load(source, null);
    }

    test "empty file" {
        _ = runTest("");
    }

    test "only empty lines" {
        _ = runTest("\n\n\n\r\n\r\n\r\n  \r\n     \r\n\t\t\r\n");
    }

    test "comments and empty lines" {
        _ = runTest(
            \\# hello
            \\   # hello, this is nice
            \\# unterminated!
        );
    }

    test "commands" {
        _ = runTest(
            \\u32
            \\u32 # with comment
            \\u32 name
            \\u16 name
            \\str 16 name
            \\type *name
            \\type (tuple tuple)
            \\type (tuple 1 2 3) (3 4 5)
            \\type (*tfoo (1 2) (3 4))
            \\type "hello, world"
            \\type ("hello, string" "foo bar")
            \\type "\"" "\'" "foo bar"
            \\type .mac .foo .bar
            \\
        );
    }

    test "basic loop" {
        const prog = runTest(
            \\.loop 10
            \\
            \\.endloop
        );
        try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
        try std.testing.expect(prog.top_level.instructions[0] == .loop);
        try std.testing.expect(prog.top_level.instructions[0].loop.variable == null);
        try std.testing.expect(prog.top_level.instructions[0].loop.body.instructions.len == 0);
    }

    test "nested basic loop" {
        const prog = runTest(
            \\.loop 10
            \\  .loop 20
            \\  .endloop
            \\.endloop
        );
        try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
        try std.testing.expect(prog.top_level.instructions[0] == .loop);
        try std.testing.expect(prog.top_level.instructions[0].loop.variable == null);
        try std.testing.expect(prog.top_level.instructions[0].loop.body.instructions.len == 1);
        try std.testing.expect(prog.top_level.instructions[0].loop.body.instructions[0] == .loop);
        try std.testing.expect(prog.top_level.instructions[0].loop.body.instructions[0].loop.variable == null);
        try std.testing.expect(prog.top_level.instructions[0].loop.body.instructions[0].loop.body.instructions.len == 0);
    }

    test "variable loop" {
        const prog = runTest(
            \\.loop 10 index
            \\
            \\.endloop
        );
        try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
        try std.testing.expect(prog.top_level.instructions[0] == .loop);
        try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    }

    test "simple if block" {
        const prog = runTest(
            \\.if foo
            \\
            \\.endif
        );
        _ = prog;
        // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
        // try std.testing.expect(prog.top_level.instructions[0] == .loop);
        // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    }

    test "comparison if block" {
        const prog = runTest(
            \\.if foo bar
            \\
            \\.endif
        );
        _ = prog;
        // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
        // try std.testing.expect(prog.top_level.instructions[0] == .loop);
        // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    }

    test "simple if-else" {
        const prog = runTest(
            \\.if foo
            \\
            \\.else
            \\
            \\.endif
        );
        _ = prog;
        // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
        // try std.testing.expect(prog.top_level.instructions[0] == .loop);
        // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    }

    test "comparison if-else" {
        const prog = runTest(
            \\.if foo bar
            \\
            \\.else
            \\
            \\.endif
        );
        _ = prog;
        // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
        // try std.testing.expect(prog.top_level.instructions[0] == .loop);
        // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    }

    // test "simple if-elseif" {
    //     const prog = runTest(
    //         \\.if foo
    //         \\
    //         \\.elseif bar
    //         \\
    //         \\.elseif bam
    //         \\
    //         \\.endif
    //     );
    //     _ = prog;
    //     // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
    //     // try std.testing.expect(prog.top_level.instructions[0] == .loop);
    //     // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    // }

    // test "comparison if-elseif" {
    //     const prog = runTest(
    //         \\.if foo 10
    //         \\
    //         \\.elseif bar 10
    //         \\
    //         \\.elseif bam 10
    //         \\
    //         \\.endif
    //     );
    //     _ = prog;
    //     // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
    //     // try std.testing.expect(prog.top_level.instructions[0] == .loop);
    //     // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    // }

    // test "simple if-elseif-else" {
    //     const prog = runTest(
    //         \\.if foo
    //         \\
    //         \\.elseif bar
    //         \\
    //         \\.elseif bam
    //         \\
    //         \\.else
    //         \\
    //         \\.endif
    //     );
    //     _ = prog;
    //     // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
    //     // try std.testing.expect(prog.top_level.instructions[0] == .loop);
    //     // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    // }

    // test "comparison if-elseif-else" {
    //     const prog = runTest(
    //         \\.if foo 10
    //         \\
    //         \\.elseif bar 10
    //         \\
    //         \\.elseif bam 10
    //         \\
    //         \\.else
    //         \\
    //         \\.endif
    //     );
    //     _ = prog;
    //     // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
    //     // try std.testing.expect(prog.top_level.instructions[0] == .loop);
    //     // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
    // }

    test "subprograms" {
        const prog = runTest(
            \\.pgm foobar
            \\  type 10
            \\  type 20
            \\  type 30
            \\.endpgm
            \\
        );
        try std.testing.expectEqual(true, prog.top_level.is_top_level);
        try std.testing.expectEqual(@as(usize, 0), prog.top_level.instructions.len);
        try std.testing.expectEqual(@as(usize, 1), prog.programs.len);
        try std.testing.expectEqualStrings("foobar", prog.programs[0].name);
        try std.testing.expectEqual(false, prog.programs[0].code.is_top_level);
        try std.testing.expectEqual(@as(usize, 3), prog.programs[0].code.instructions.len);
    }
};

comptime {
    _ = Ast;
}
