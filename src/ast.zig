const std = @import("std");
const ptk = @import("parser-toolkit");
const mainfile = @import("main.zig");

const allo = mainfile.allo;

pub const Script = struct {
    top_level: Sequence,
    programs: []Program,
};

pub const Sequence = struct {
    is_top_level: bool,
    instructions: []Node,
};

pub const Node = union(enum) {
    command: Command,
    decision: Conditional,
    loop: Loop,
    breakloop,
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
    number: []const u8, // 10, 24.5
    variable_ref: []const u8, // *foo
    string: []const u8, // "hello, world"-
    tuple: []ValueToken, // ( <token> <token> <token> )
    identifier: []const u8, // the rest: hello, foo_bar
};

const TokenType = enum {
    macro,
    identifier,
    @"(",
    @")",
    star_ref,
    string,
    number_literal,
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
    Pattern.create(.number_literal, ptk.matchers.sequenceOf(.{ ptk.matchers.literal("0b"), ptk.matchers.binaryNumber })),
    Pattern.create(.number_literal, ptk.matchers.sequenceOf(.{ ptk.matchers.literal("0x"), ptk.matchers.hexadecimalNumber })),
    Pattern.create(.number_literal, ptk.matchers.sequenceOf(.{ ptk.matchers.literal("0o"), ptk.matchers.octalNumber })),
    Pattern.create(.number_literal, ptk.matchers.sequenceOf(.{ ptk.matchers.literal("-"), ptk.matchers.decimalNumber, ptk.matchers.literal("."), ptk.matchers.decimalNumber })),
    Pattern.create(.number_literal, ptk.matchers.sequenceOf(.{ ptk.matchers.literal("-"), ptk.matchers.decimalNumber })),
    Pattern.create(.number_literal, ptk.matchers.sequenceOf(.{ ptk.matchers.decimalNumber, ptk.matchers.literal("."), ptk.matchers.decimalNumber })),
    Pattern.create(.number_literal, ptk.matchers.decimalNumber),
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
    const source_code = std.fs.cwd().readFileAlloc(allo, file_name, 1 << 20) catch @panic("oom");

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

fn parseConditionBlock(stream: *Tokenizer) Conditional {
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
        return Conditional{
            .value = value,
            .comparison = comparison,
            .true_body = true_block,
            .false_body = null,
        };
    }
    if (if_terminator == .@"else") {
        endOfLine(stream); // terminate .else
        const false_block = parseSequence(undefined, stream, isEndOrElseOrElseIf);
        endOfLine(stream); // terminate .endif

        return Conditional{
            .value = value,
            .comparison = comparison,
            .true_body = true_block,
            .false_body = false_block,
        };
    }

    if (if_terminator == .@"elseif") {
        // transform .elseif into a
        //
        // .else
        //  .if new …
        //  .endif
        // .endif # this is implicit from the previous one
        const false_block = parseConditionBlock(stream);

        const seq = allo.alloc(Node, 1) catch @panic("oom");
        seq[0] = Node{ .decision = false_block };

        return Conditional{
            .value = value,
            .comparison = comparison,
            .true_body = true_block,
            .false_body = Sequence{ .instructions = seq, .is_top_level = false },
        };
    }
    std.debug.panic("invalid terminator: {}\n", .{if_terminator});
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
                        var block = parseConditionBlock(stream);
                        list.append(Node{ .decision = block }) catch @panic("oom");
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

            .@"(", .@")", .star_ref, .string, .number_literal => std.debug.panic("illegal start of command: '{s}'", .{first_token.text}),
            .line_feed => continue, // empty line
            .whitespace, .comment => unreachable,
        }
    }
}

fn parseTokenValue(stream: *Tokenizer, item: Token) ValueToken {
    return switch (item.type) {
        .macro, .identifier => ValueToken{ .identifier = item.text },
        .number_literal => ValueToken{ .number = item.text },
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
    mainfile.arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
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

test "simple if-elseif" {
    const prog = runTest(
        \\.if foo
        \\
        \\.elseif bar
        \\
        \\.elseif bam
        \\
        \\.endif
    );
    _ = prog;
    // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
    // try std.testing.expect(prog.top_level.instructions[0] == .loop);
    // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
}

test "comparison if-elseif" {
    const prog = runTest(
        \\.if foo 10
        \\
        \\.elseif bar 10
        \\
        \\.elseif bam 10
        \\
        \\.endif
    );
    _ = prog;
    // try std.testing.expectEqual(@as(usize, 1), prog.top_level.instructions.len);
    // try std.testing.expect(prog.top_level.instructions[0] == .loop);
    // try std.testing.expectEqualStrings("index", prog.top_level.instructions[0].loop.variable.?);
}

test "simple if-elseif-else" {
    const prog = runTest(
        \\.if foo
        \\
        \\.elseif bar
        \\
        \\.elseif bam
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

test "comparison if-elseif-else" {
    const prog = runTest(
        \\.if foo 10
        \\
        \\.elseif bar 10
        \\
        \\.elseif bam 10
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
