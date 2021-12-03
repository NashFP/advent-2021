const std = @import("std");

fn calculate(numbers: []const u16) !u32 {
    var gamma: u16 = 0;
    var epsilon: u16 = 0;

    var i: u4 = 0;
    while (true) : (i += 1) {
        const mask = @as(u16, 1) << i;

        var zeroes: usize = 0;
        var ones: usize = 0;
        for (numbers) |n| {
            if (n & mask == 0) {
                zeroes += 1;
            } else {
                ones += 1;
            }
        }

        if (zeroes > ones and ones > 0) {
            epsilon = epsilon | mask;
        } else if (ones > zeroes) {
            gamma = gamma | mask;
        }

        if (i == 15) break;
    }

    return @as(u32, gamma) * @as(u32, epsilon);
}

fn run(input: []const u8) !u32 {
    var numbers: [1024]u16 = undefined;
    var count: usize = 0;

    var tokens = std.mem.tokenize(u8, input, " \n");
    while (tokens.next()) |t| {
        numbers[count] = try std.fmt.parseInt(u16, t, 2);
        count += 1;
        if (count == 1024) return error.TooManyNumbers;
    }

    return try calculate(numbers[0..count]);
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
