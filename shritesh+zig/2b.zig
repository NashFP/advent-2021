const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const print = std.debug.print;

const stdin = std.io.getStdIn().reader();

pub fn main() !void {
    var buffer: [16]u8 = undefined;

    var position: i32 = 0;
    var depth: i32 = 0;
    var aim: i32 = 0;

    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var tokens = mem.tokenize(u8, line, " \n");
        const direction = tokens.next() orelse return error.DirectionParseError;
        const amount_s = tokens.next() orelse return error.AmountReadError;
        const amount = try fmt.parseInt(u8, amount_s, 10);

        if (mem.eql(u8, direction, "forward")) {
            position += amount;
            depth += aim * amount;
        } else if (mem.eql(u8, direction, "down")) {
            aim += amount;
        } else if (mem.eql(u8, direction, "up")) {
            aim -= amount;
        } else return error.InvalidDirectionError;
    }

    print("{}\n", .{position * depth});
}
