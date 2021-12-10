const std = @import("std");

test "example" {
    const input = @embedFile("10_example.txt");
    try std.testing.expectEqual(@as(usize, 288957), try run(input));
}

pub fn main() !void {
    const input = @embedFile("10.txt");
    std.debug.print("{}\n", .{try run(input)});
}

fn run(input: []const u8) !usize {
    var scores = try std.BoundedArray(usize, 100).init(0);

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line|
        if (try score(line)) |s|
            try scores.append(s);

    std.sort.sort(usize, scores.slice(), {}, comptime std.sort.asc(usize));
    return scores.get(scores.len / 2);
}

fn score(line: []const u8) !?usize {
    var stack = try std.BoundedArray(u8, 32).init(0);

    for (line) |c| switch (c) {
        '(', '[', '{', '<' => try stack.append(c),
        ')' => if (stack.pop() != '(') return null,
        ']' => if (stack.pop() != '[') return null,
        '}' => if (stack.pop() != '{') return null,
        '>' => if (stack.pop() != '<') return null,
        else => unreachable,
    };

    var sum: usize = 0;
    while (stack.popOrNull()) |c| {
        sum *= 5;
        switch (c) {
            '(' => sum += 1,
            '[' => sum += 2,
            '{' => sum += 3,
            '<' => sum += 4,
            else => unreachable,
        }
    }

    return sum;
}
