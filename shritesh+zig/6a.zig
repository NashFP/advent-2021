const std = @import("std");

const FishSchool = std.ArrayList(u4);

test "example" {
    const allocator = std.testing.allocator;
    const input = @embedFile("6_example.txt");
    const result = try run(allocator, input);
    try std.testing.expectEqual(@as(usize, 5934), result);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const input = @embedFile("6.txt");
    const result = try run(allocator, input);
    std.debug.print("{}\n", .{result});
}

fn run(allocator: *std.mem.Allocator, input: []const u8) !usize {
    var fishes = FishSchool.init(allocator);
    defer fishes.deinit();

    var numbers = std.mem.split(u8, input, ",");
    while (numbers.next()) |n| {
        const number = try std.fmt.parseInt(u4, n, 10);
        try fishes.append(number);
    }

    var day: usize = 0;
    while (day < 80) : (day += 1) {
        for (fishes.items) |*f| switch (f.*) {
            0 => {
                f.* = 6;
                try fishes.append(8);
            },
            else => f.* -= 1,
        };
    }

    return fishes.items.len;
}
