const std = @import("std");

fn run(input: []const u8) !u32 {
    var zeroes: [16]u16 = .{0} ** 16;
    var ones: [16]u16 = .{0} ** 16;
    var len: usize = 0;

    var tokens = std.mem.tokenize(u8, input, " \n");
    while (tokens.next()) |t| {
        len = t.len;

        for (t) |c, i| {
            if (c == '0') {
                zeroes[i] += 1;
            } else if (c == '1') {
                ones[i] += 1;
            } else return error.InvalidCharacter;
        }
    }

    var gamma: u32 = 0;
    var epsilon: u32 = 0;

    var i: usize = 0;
    while (i < len) : (i += 1) {
        const mask = @as(u32, 1) << @intCast(u4, len - i - 1);
        if (zeroes[i] > ones[i]) {
            epsilon = epsilon | mask;
        } else if (ones[i] > zeroes[i]) {
            gamma = gamma | mask;
        }
    }

    return gamma * epsilon;
}

pub fn main() !void {
    const input = @embedFile("3.txt");
    const result = try run(input);
    std.debug.print("{}\n", .{result});
}

test "example" {
    const input = @embedFile("3_example.txt");
    const result = try run(input);
    try std.testing.expectEqual(@as(u32, 198), result);
}
