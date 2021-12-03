const std = @import("std");

const max_nums = 1024;

fn common(most: bool, numbers: [][]const u8, to_consider: []const bool, idx: usize) ?u8 {
    var count: isize = 0;
    for (numbers) |n, i| {
        if (!to_consider[i]) continue;
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
    var buffer: [max_nums]bool = .{true} ** max_nums;
    var to_consider = buffer[0..numbers.len];
    var remaining = numbers.len;

    loop: for (numbers[0]) |_, i| {
        var bit = common(most, numbers, to_consider, i) orelse tie;

        for (to_consider) |*tc, n| {
            if (!tc.*) continue;
            if (numbers[n][i] != bit) {
                tc.* = false;
                remaining -= 1;
            }

            if (remaining == 1) break :loop;
        }
    } else if (remaining != 1) return error.NotFound;

    for (to_consider) |tc, idx| {
        if (tc) return std.fmt.parseInt(u32, numbers[idx], 2);
    } else unreachable;
}

fn run(input: []const u8) !u32 {
    var numbers: [max_nums][]const u8 = undefined;
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
