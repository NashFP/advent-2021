const std = @import("std");
const expect = std.testing.expect;

fn run(input: []const u8) !u32 {
    var zeroes: [16]u16 = .{0} ** 16;
    var ones: [16]u16 = .{0} ** 16;

    var len: ?usize = undefined;

    var tokens = std.mem.tokenize(u8, input, " \n");
    while (tokens.next()) |t| {
        if (len) |l| {
            if (l != t.len) return error.DifferentSizedIntegers;
        }
        len = t.len;

        for (t) |c, i| {
            if (c == '0') {
                zeroes[i] += 1;
            } else if (c == '1') {
                ones[i] += 1;
            } else return error.InvalidCharacter;
        }
    }

    var count = len orelse return error.EmptyInput;
    var gamma: [16]u8 = undefined;
    var epsilon: [16]u8 = undefined;

    var i: usize = 0;
    while (i < count) : (i += 1) {
        if (zeroes[i] > ones[i]) {
            gamma[i] = '0';
            epsilon[i] = '1';
        } else if (zeroes[i] < ones[i]) {
            gamma[i] = '1';
            epsilon[i] = '0';
        } else {
            return error.EqualCountError;
        }
    }

    const g = try std.fmt.parseInt(u32, gamma[0..count], 2);
    const e = try std.fmt.parseInt(u32, epsilon[0..count], 2);

    return g * e;
}

pub fn main() !void {
    const input = @embedFile("3.txt");
    const result = try run(input);
    std.debug.print("{}\n", .{result});
}

test "example" {
    const input = @embedFile("3_example.txt");
    const result = try run(input);
    try expect(result == 198);
}
