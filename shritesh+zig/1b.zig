const std = @import("std");
const fmt = std.fmt;
const print = std.debug.print;

const stdin = std.io.getStdIn().reader();

pub fn main() !void {
    var buffer: [4]u8 = undefined;

    var first: ?u16 = null;
    var second: ?u16 = null;

    var last: ?u16 = null;

    var increments: usize = 0;

    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const third = try fmt.parseInt(u16, line, 10);

        if (first) |f| {
            if (second) |s| {
                const sum = f + s + third;

                if (last) |l| {
                    if (sum > l) {
                        increments += 1;
                    }
                }

                last = sum;
            }
        }

        first = second;
        second = third;
    }

    print("{}\n", .{increments});
}
