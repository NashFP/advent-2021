const std = @import("std");

test "example" {
    const input = @embedFile("6_example.txt");
    const result80 = try run(input, 80);
    const result256 = try run(input, 256);
    try std.testing.expectEqual(@as(usize, 5934), result80);
    try std.testing.expectEqual(@as(usize, 26984457539), result256);
}

pub fn main() !void {
    const input = @embedFile("6.txt");
    const result80 = try run(input, 80);
    const result256 = try run(input, 256);
    std.debug.print("80 days: {}\n256 days: {}\n", .{ result80, result256 });
}

fn run(input: []const u8, comptime iterations: usize) !usize {
    var count: usize = 0;

    var births: [iterations]usize = .{0} ** iterations;

    var numbers = std.mem.split(u8, input, ",");
    while (numbers.next()) |n| {
        count += 1;
        var day = try std.fmt.parseInt(usize, n, 10);
        while (day < births.len) : (day += 7) { // New fish every 7 days
            births[day] += 1;
        }
    }

    for (births) |b, i| {
        if (b == 0) continue;
        count += b;

        var d = i + 9; // 2 more days in the first cycle
        while (d < births.len) : (d += 7) { // 7 days as usual after that
            births[d] += b;
        }
    }

    return count;
}
