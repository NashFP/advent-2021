const std = @import("std");
const fmt = std.fmt;
const print = std.debug.print;

const stdin = std.io.getStdIn().reader();

pub fn main() !void {
    var buffer: [4]u8 = undefined;
    var depths: [2000]u16 = undefined;
    var idx: usize = 0;

    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        depths[idx] = try fmt.parseInt(u16, line, 10);
        idx += 1;
    }

    print("{}\n", .{countIncrements(depths[0..idx])});
}

fn countIncrements(depths: []const u16) usize {
    var i: usize = 0;
    var increments: usize = 0;
    var last: ?u16 = null;

    while (i + 2 < depths.len) : (i += 1) {
        const sum = depths[i] + depths[i + 1] + depths[i + 2];

        if (last) |l| {
            if (sum > l)
                increments += 1;
        }

        last = sum;
    }

    return increments;
}
