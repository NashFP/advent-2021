const std = @import("std");
const Stack = std.BoundedArray(u8, 32);
const List = std.BoundedArray(usize, 100);

test "example" {
    const input = @embedFile("10_example.txt");
    try std.testing.expectEqual(@as(usize, 288957), try run(input));
}

pub fn main() !void {
    const input = @embedFile("10.txt");
    std.debug.print("{}\n", .{try run(input)});
}

fn run(input: []const u8) !usize {
    var stack = try Stack.init(0);
    var scores = try List.init(0);

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        const n = try score(line, &stack);
        if (n > 0) {
            try scores.append(n);
        }
    }

    std.sort.sort(usize, scores.slice(), {}, comptime std.sort.asc(usize));
    return scores.get(scores.len / 2);
}

fn score(line: []const u8, stack: *Stack) !usize {
    try stack.resize(0);

    for (line) |c| switch (c) {
        '(', '[', '{', '<' => try stack.append(c),
        ')' => if (stack.pop() != '(') return 0,
        ']' => if (stack.pop() != '[') return 0,
        '}' => if (stack.pop() != '{') return 0,
        '>' => if (stack.pop() != '<') return 0,
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
