const std = @import("std");
const io = std.io;

const stdin = io.getStdIn().reader();
const stdout = io.getStdOut().writer();
const stderr = io.getStdErr().writer();

const OpType = enum {
    MOVE,
    ADD,
    IN,
    OUT,
    LOOPS,
    LOOPE,

    // optimization related opcodes
    CLEAR,
    MUL,
};

const OpCode = struct {
    type: OpType,
    args: [2]isize,

    pub fn new(op: OpType, a: isize, b: isize) OpCode {
        return .{ .type = op, .args = [_]isize{ a, b } };
    }

    // disassembling

    fn noArgs(_: *const OpCode, name: []const u8, writer: anytype) !void {
        _ = try writer.write(name);
    }

    fn oneArg(self: *const OpCode, name: []const u8, writer: anytype) !void {
        _ = try writer.print("{s}\t{}", .{ name, self.args[0] });
    }

    fn twoArgs(self: *const OpCode, name: []const u8, writer: anytype) !void {
        _ = try writer.print("{s}\t{},{}", .{ name, self.args[0], self.args[1] });
    }

    pub fn disassemble(self: *const OpCode, i: usize, writer: anytype) !void {
        try writer.print("{}\t| ", .{i});
        try switch (self.type) {
            .MOVE => self.oneArg("MOVE", writer),
            .ADD => self.oneArg("ADD", writer),
            .IN => self.noArgs("IN", writer),
            .OUT => self.noArgs("OUT", writer),
            .LOOPS => self.oneArg("LOOPS", writer),
            .LOOPE => self.oneArg("LOOPE", writer),
            .CLEAR => self.noArgs("CLEAR", writer),
            .MUL => self.twoArgs("MUL", writer),
        };
        try writer.writeByte('\n');
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

pub fn bfvm(maxCells: usize) type {
    return struct {
        const Self = @This();

        cells: [maxCells]u8 = [_]u8{0} ** maxCells,
        ptr: usize = 0,
        code: std.ArrayList(OpCode) = std.ArrayList(OpCode).init(allocator),
        ip: usize = 0,
        output: std.ArrayList(u8) = std.ArrayList(u8).init(allocator),

        pub fn addOp(vm: *Self, op: OpType) void {
            vm.addOp2(op, 0, 0);
        }

        pub fn addOp1(vm: *Self, op: OpType, arg: isize) void {
            vm.addOp2(op, arg, 0);
        }

        pub fn addOp2(vm: *Self, op: OpType, a: isize, b: isize) void {
            vm.code.append(OpCode.new(op, a, b)) catch
                @panic("couldn't append to code array in vm");
        }

        const CompileError = error{
            UnclosedLoop,
            StrayRightBracket,
        };

        pub fn compile(vm: *Self, code: []const u8) !void {
            {
                var loopStack = std.ArrayList(isize).init(allocator);
                defer loopStack.deinit();

                var acc: isize = 0;
                var clump = false;
                var moveIns = false;

                for (code) |c| {
                    if (clump) {
                        if (moveIns != (c == '>' or c == '<')) {
                            clump = false;
                        } else {
                            switch (c) {
                                '+', '>' => acc += 1,
                                '-', '<' => acc -= 1,
                                else => clump = false,
                            }
                        }

                        if (!(clump or acc == 0)) {
                            vm.addOp1(if (moveIns) .MOVE else .ADD, acc);
                        }
                    }

                    if (!clump) {
                        moveIns = (c == '>' or c == '<');
                        switch (c) {
                            '+', '>' => {
                                clump = true;
                                acc = 1;
                            },
                            '-', '<' => {
                                clump = true;
                                acc = -1;
                            },
                            '[' => {
                                loopStack.append(@intCast(vm.code.items.len)) catch
                                    @panic("couldn't append to loop stack");

                                vm.addOp1(.LOOPS, std.math.maxInt(isize));
                            },
                            ']' => {
                                const s = loopStack.popOrNull() orelse
                                    return CompileError.StrayRightBracket;

                                vm.addOp1(.LOOPE, s + 1);
                                // patch topmost loop
                                vm.code.items[@intCast(s)].args[0] = @intCast(vm.code.items.len);
                            },
                            ',' => vm.addOp(.IN),
                            '.' => vm.addOp(.OUT),
                            else => {}, // comments
                        }
                    }
                }

                if (loopStack.items.len != 0) return CompileError.UnclosedLoop;
            }

            try vm.optimize();
        }

        fn scanMultiply(_: *Self, code: *std.ArrayList(OpCode), i: usize) !usize {
            const items = code.items;
            const start = i;
            var end = i;

            if (end >= items.len) return start;
            if (items[i].type != .LOOPS) return start;

            var ptrOff: isize = 0;
            var maxOff: isize = 0;
            var factor: isize = 0;

            const loope = while (true) {
                end += 1;
                if (end >= items.len) return start;

                const ins = items[end];
                switch (ins.type) {
                    .MOVE => ptrOff += ins.args[0],
                    .ADD => {
                        if (ins.args[0] > 0)
                            factor += ins.args[0]
                        else {
                            end += 1;
                            const loope = &items[end];
                            if (loope.type != .LOOPE) return start;
                            end += 1;
                            break loope.args[0];
                        }
                    },
                    else => return start,
                }
                if (@abs(ptrOff) > @abs(maxOff)) maxOff = ptrOff;
            };

            std.debug.assert(loope == start + 1);

            if (ptrOff != 0) return start;

            const mul = OpCode.new(.MUL, factor, maxOff);
            try code.replaceRange(start, end - start, &[_]OpCode{mul});

            return i + 1;
        }

        pub fn optimize(vm: *Self) !void {
            const code = &vm.code;
            const items = code.items;
            var i: usize = 0;

            while (true) {
                if (i >= items.len) break;

                const a = items[i];
                switch (a.type) {
                    .LOOPS => {
                        const b = items[i + 1];
                        const optype = b.type;
                        if (optype == .ADD and items[i + 2].type == .LOOPE) {
                            // attempt to clear
                            const op = &[_]OpCode{OpCode.new(.CLEAR, 0, 0)};
                            try code.replaceRange(i, 3, op);
                        } else {
                            i = try vm.scanMultiply(code, i);
                        }
                    },
                    else => {},
                }

                i += 1;
            }
        }

        pub fn disassemble(vm: *Self) !void {
            const writer = io.getStdErr().writer();
            for (vm.code.items, 0..) |op, i| try op.disassemble(i, writer);
        }

        const RuntimeResult = error{EndOfProgram};

        pub fn step(vm: *Self) !void {
            if (vm.ip >= vm.code.items.len) return RuntimeResult.EndOfProgram;

            const cur = vm.code.items[vm.ip];
            vm.ip += 1;
            switch (cur.type) {
                .MOVE => vm.ptr = @intCast(@as(isize, @intCast(vm.ptr)) + cur.args[0]),
                .ADD => {
                    // *why*
                    const amt: i8 = @truncate(cur.args[0]);
                    const cell: i8 = @intCast(vm.cells[vm.ptr]);
                    vm.cells[vm.ptr] = @as(u8, @intCast(@as(i8, @truncate(@addWithOverflow(cell, amt)[0]))));
                },
                .IN => vm.cells[vm.ptr] = try stdin.readByte(),
                .OUT => try vm.output.append(vm.cells[vm.ptr]),
                .LOOPS => if (vm.cells[vm.ptr] == 0) {
                    vm.ip = @as(usize, @intCast(cur.args[0]));
                },
                .LOOPE => if (vm.cells[vm.ptr] != 0) {
                    vm.ip = @as(usize, @intCast(cur.args[0]));
                },
                .CLEAR => vm.cells[vm.ptr] = 0,
                .MUL => {
                    const offset: isize = cur.args[1];
                    const amt: u8 = @truncate(@as(usize, @intCast(cur.args[0])));
                    vm.cells[@as(usize, @intCast(@as(isize, @intCast(vm.ptr)) + offset))] =
                        @truncate(@mulWithOverflow(vm.cells[vm.ptr], amt)[0]);
                    vm.cells[vm.ptr] = 0;
                },
            }
        }

        pub fn run(vm: *Self) !void {
            while (true) {
                vm.step() catch |e|
                    if (e == RuntimeResult.EndOfProgram) return;
            }
        }

        // pub fn clearOutput(vm: *Self) void {
        //     vm.output.clearAndFree();
        // }
    };
}

const clap = @import("clap");

const params = clap.parseParamsComptime(
    \\-h, --help
    \\  show this text and exit
    \\-c, --compile
    \\  send compiled code to output.
    \\  if this flag is passed, <output> must also be passed in
    \\<input>
    \\  the input file. this is mandatory.
    \\<output>
    \\  if passed, will send output to this file.
    \\  otherwise, prints to stdout.
    \\
);

pub fn printUsage() !void {
    const programName: []const u8 = r: {
        var args = try std.process.argsWithAllocator(allocator);
        defer args.deinit();
        break :r args.next() orelse unreachable;
    };

    var usage = std.ArrayList(u8).init(allocator);
    defer usage.deinit();
    var w = usage.writer();

    try w.print(
        \\zbf: an optimizing Brainfuck interpreter written in Zig
        \\by @thacuber2a03
        \\
        \\usage: {s} 
    , .{programName});
    try clap.usage(w, clap.Help, &params);
    _ = try w.write("\n");

    try clap.help(w, clap.Help, &params, .{
        .markdown_lite = false,
        .indent = 2,
        .description_on_new_line = true,
        .description_indent = 2,
        .spacing_between_parameters = 0,
    });

    _ = try stderr.write(usage.items);
}

pub fn main() !void {
    const parsers = comptime .{
        .input = clap.parsers.string,
        .output = clap.parsers.string,
    };

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        diag.report(stderr, err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0)
        return printUsage();
}

const expect = std.testing.expect;
const testingvm = bfvm(30000);

test "basic interpret" {
    var vm = testingvm{};

    try vm.compile("+++++++[>++++++<-]>+.++++.--.");
    // try stderr.writeByte('\n');
    // try vm.disassemble();
    try vm.run();
    try expect(std.mem.eql(u8, vm.output.items, "+/-"));
}

fn testOpcode2(vm: *testingvm, i: usize, optype: OpType, a: usize, b: usize) !void {
    try expect(vm.code.items.len > i);
    const op = vm.code.items[i];
    try expect(op.type == optype);
    try expect(op.args[0] == a);
    try expect(op.args[1] == b);
}

fn testOpcode1(vm: *testingvm, i: usize, optype: OpType, arg: usize) !void {
    return testOpcode2(vm, i, optype, arg, 0);
}

fn testOpcode(vm: *testingvm, i: usize, optype: OpType) !void {
    return testOpcode2(vm, i, optype, 0, 0);
}

test "clear optimization" {
    var vm = testingvm{};

    try vm.compile("[-][+]");
    try testOpcode(&vm, 0, .CLEAR);
    try testOpcode(&vm, 1, .CLEAR);

    vm.cells[0] = 42;
    try vm.step();
    try expect(vm.cells[0] == 0);
    vm.cells[0] = 42;
    try vm.step();
    try expect(vm.cells[0] == 0);
}

test "multiplication optimization" {
    var vm = testingvm{};

    try vm.compile("+++++[>+++++<-]");
    try testOpcode1(&vm, 0, .ADD, 5);
    try testOpcode2(&vm, 1, .MUL, 5, 1);

    try vm.step();
    try expect(vm.cells[0] == 5);
    try expect(vm.cells[1] == 0);
    try vm.step();
    try expect(vm.cells[0] == 0);
    try expect(vm.cells[1] == 25);
}
