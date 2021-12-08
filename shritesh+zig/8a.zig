const std = @import("std");

test "example" {
    const input = @embedFile("8_example.txt");
    const result = run(input);
    try std.testing.expectEqual(@as(usize, 26), result);
}

pub fn main() void {
    const input = @embedFile("8.txt");
    const result = run(input);
    std.debug.print("{}\n", .{result});
}

fn run(input: []const u8) usize {
    var count: usize = 0;

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        var pipe_index = std.mem.indexOfScalar(u8, line, '|').?;
        const numbers_substring = line[pipe_index + 2 ..];

        var wires = std.mem.tokenize(u8, numbers_substring, " ");
        while (wires.next()) |wire| {
            const n = wiresToDigit(wire);
            switch (@popCount(u7, n)) {
                correct_counts[1], correct_counts[4], correct_counts[7], correct_counts[8] => count += 1,
                else => {},
            }
        }
    }

    return count;
}

const correct_digits = [_]u7{
    //g_f_e_d_c_b_a
    0b1_1_1_0_1_1_1, // 0
    0b0_1_0_0_1_0_0, // 1
    0b1_0_1_1_1_0_1, // 2
    0b1_1_0_1_1_0_1, // 3
    0b0_1_0_1_1_1_0, // 4
    0b1_1_0_1_0_1_1, // 5
    0b1_1_1_1_0_1_1, // 6
    0b0_1_0_0_1_0_1, // 7
    0b1_1_1_1_1_1_1, // 8
    0b1_1_0_1_1_1_1, // 9
};

const correct_counts = counts: {
    var counts: [10]usize = undefined;
    inline for (correct_digits) |d, i| {
        counts[i] = @popCount(u7, d);
    }
    break :counts counts;
};

fn wiresToDigit(wires: []const u8) u7 {
    var digit: u7 = 0;
    for (wires) |c| {
        if (c < 'a' or c > 'g') unreachable;
        const mask = @as(u7, 1) << @intCast(u3, c - 'a');
        digit |= mask;
    }

    return digit;
}
