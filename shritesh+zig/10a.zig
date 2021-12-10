const std = @import("std");

test "example" {
    const input = @embedFile("10_example.txt");
    try std.testing.expectEqual(@as(usize, 26397), try run(input));
}

pub fn main() !void {
    const input = @embedFile("10.txt");
    std.debug.print("{}\n", .{try run(input)});
}

fn run(input: []const u8) !usize {
    var total: usize = 0;

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line|
        total += try score(line);

    return total;
}

fn score(line: []const u8) !usize {
    var stack = try std.BoundedArray(u8, 32).init(0);

    for (line) |c| switch (c) {
        '(', '[', '{', '<' => try stack.append(c),
        ')' => if (stack.pop() != '(') return 3,
        ']' => if (stack.pop() != '[') return 57,
        '}' => if (stack.pop() != '{') return 1197,
        '>' => if (stack.pop() != '<') return 25137,
        else => unreachable,
    } else return 0;
}
