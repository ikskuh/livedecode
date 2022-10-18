const std = @import("std");

const allo = std.heap.c_allocator;

fn trim(str: []const u8) []const u8 {
    return std.mem.trim(u8, str, " \r\n");
}

pub fn main() !void {
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
        .fields = std.StringHashMap(Value).init(allo),
        .programs = std.StringHashMap(std.ArrayList([]const u8)).init(allo),
    };

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
    var cmditer = std.mem.tokenize(u8, line, ": ");
    const cmd = cmditer.next().?;
    inline for (@typeInfo(Commands).Struct.decls) |decl| {
        if (std.mem.eql(u8, decl.name, cmd)) {
            try @field(Commands, decl.name)(state, &cmditer);
            return;
        }
    }

    // type name
    const val_type = std.meta.stringToEnum(Type, cmd) orelse std.debug.panic("unknown type: {s}", .{cmd});

    const reader = state.file.reader();
    const value: Value = switch (val_type) {
        .u8 => Value{ .u8 = try reader.readIntLittle(u8) },
        .u16 => Value{ .u16 = try reader.readIntLittle(u16) },
        .u32 => Value{ .u32 = try reader.readIntLittle(u32) },
        .u64 => Value{ .u64 = try reader.readIntLittle(u64) },
        .i8 => Value{ .i8 = try reader.readIntLittle(i8) },
        .i16 => Value{ .i16 = try reader.readIntLittle(i16) },
        .i32 => Value{ .i32 = try reader.readIntLittle(i32) },
        .i64 => Value{ .i64 = try reader.readIntLittle(i64) },
        .f32 => Value{ .f32 = @bitCast(f32, try reader.readIntLittle(u32)) },
        .f64 => Value{ .f64 = @bitCast(f64, try reader.readIntLittle(u64)) },
        .str => blk: {
            const len = try state.decodeInt(cmditer.next().?);

            var mem = try allo.alloc(u8, len);

            try reader.readNoEof(mem);

            break :blk Value{ .str = mem };
        },
        .blob => blk: {
            const len = try state.decodeInt(cmditer.next().?);

            var mem = try allo.alloc(u8, len);

            try reader.readNoEof(mem);

            break :blk Value{ .blob = mem };
        },
    };

    if (cmditer.next()) |name| {
        std.debug.print("{s: >20} = {}\n", .{ name, value });
        try state.fields.put(try allo.dupe(u8, name), value);
    }
}

const Args = std.mem.TokenIterator(u8);

const Commands = struct {
    pub fn print(state: *State, iter: *Args) !void {
        while (iter.next()) |item| {
            var val = state.decode(item) catch Value{ .str = item };
            std.debug.print("{} ", .{val});
        }
        std.debug.print("\n", .{});
    }

    pub fn seek(state: *State, iter: *Args) !void {
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

    pub fn tell(state: *State, iter: *Args) !void {
        const where = try state.file.getPos();
        if (iter.next()) |value| {
            try state.fields.put(try allo.dupe(u8, value), .{ .u64 = where });
        }
        std.debug.print("current file position: {}\n", .{where});
    }

    pub fn dump(state: *State, iter: *Args) !void {
        const len = try state.decodeInt(iter.next().?);

        var line_buffer: [16]u8 = undefined;

        var i: u64 = 0;
        var where = try state.file.getPos();

        while (i < len) : (i += line_buffer.len) {
            const bytes = std.math.min(line_buffer.len, len - i);
            const actual = try state.file.read(line_buffer[0..bytes]);

            std.debug.print("0x{X:0>8}:", .{where});

            for (line_buffer[0..actual]) |c, j| {
                if (j == line_buffer.len / 2) std.debug.print(" ", .{});
                std.debug.print(" {X:0>2}", .{c});
            }
            for (line_buffer[actual..]) |_, j| {
                if ((actual + j) == line_buffer.len / 2) std.debug.print(" ", .{});
                std.debug.print(" __", .{});
            }

            std.debug.print(" |", .{});

            for (line_buffer[0..actual]) |c| {
                std.debug.print("{c}", .{
                    if (std.ascii.isPrint(c)) c else '.',
                });
            }
            for (line_buffer[actual..]) |_| {
                std.debug.print(" ", .{});
            }

            std.debug.print("|\n", .{});

            where += actual;
            if (actual < bytes)
                break;
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

    // replay name
    pub fn replay(state: *State, iter: *Args) !void {
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
};

const Program = std.ArrayList([]const u8);

const State = struct {
    file: std.fs.File,
    fields: std.StringHashMap(Value),
    programs: std.StringHashMap(Program),

    current_pgm: ?*Program = null,

    fn decodeInt(state: State, str: []const u8) !u64 {
        var val = try state.decode(str);
        return val.getInt();
    }

    fn decode(state: State, str: []const u8) !Value {
        return if (std.mem.startsWith(u8, str, "*"))
            state.fields.get(str[1..]) orelse @panic("var not found")
        else
            Value{ .u64 = try std.fmt.parseInt(u64, str, 0) };
    }
};

const Value = union(enum) {
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

const Type = std.meta.Tag(Value);
