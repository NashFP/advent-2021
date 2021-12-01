const std = @import("std");
const fmt = std.fmt;
const print = std.debug.print;

const stdin = std.io.getStdIn().reader();

pub fn main() !void {
    var buffer: [4]u8 = undefined;

    var last: ?u16 = null;
    var increments: u16 = 0;

    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const depth = try fmt.parseInt(u16, line, 10);

        if (last) |l| {
            if (depth > l)
                increments += 1;
        }

        last = depth;
    }

    print("{}\n", .{increments});
}
