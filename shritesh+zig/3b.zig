const std = @import("std");

inline fn isSet(set: u1024, idx: usize) bool {
    const mask = @as(u1024, 1) << @intCast(u10, idx);
    return set & mask != 0;
}

inline fn setBit(set: *u1024, idx: usize) void {
    const mask = @as(u1024, 1) << @intCast(u10, idx);
    set.* = set.* | mask;
}

fn common(most: bool, numbers: [][]const u8, skip: u1024, idx: usize) ?u8 {
    var count: isize = 0;
    for (numbers) |n, i| {
        if (isSet(skip, i)) continue;
        switch (n[idx]) {
            '0' => count -= 1,
            '1' => count += 1,
            else => unreachable,
        }
    }

    if (count < 0) {
        if (most) return '0' else return '1';
    } else if (count > 0) {
        if (most) return '1' else return '0';
    } else return null;
}

fn criteria(numbers: [][]const u8, tie: u8, most: bool) !u32 {
    var skip: u1024 = 0;
    var remaining = numbers.len;

    loop: for (numbers[0]) |_, i| {
        var bit = common(most, numbers, skip, i) orelse tie;

        for (numbers) |n, j| {
            if (isSet(skip, j)) continue;

            if (n[i] != bit) {
                setBit(&skip, j);
                remaining -= 1;
            }

            if (remaining == 1) break :loop;
        }
    } else return error.NotFound;

    return std.fmt.parseInt(u32, numbers[@ctz(u1024, ~skip)], 2);
}

fn run(input: []const u8) !u32 {
    var numbers: [1024][]const u8 = undefined;
    var count: usize = 0;

    var tokens = std.mem.tokenize(u8, input, "\n ");
    while (tokens.next()) |t| {
        numbers[count] = t;
        count += 1;
    }

    const oxygen = try criteria(numbers[0..count], '1', true);
    const co2 = try criteria(numbers[0..count], '0', false);

    return oxygen * co2;
}

pub fn main() !void {
    const input = @embedFile("3.txt");
    const result = try run(input);
    std.debug.print("{}\n", .{result});
}

test "example" {
    const input = @embedFile("3_example.txt");
    const result = try run(input);
    try std.testing.expectEqual(@as(u32, 230), result);
}
