const std = @import("std");
const Rules = std.BoundedArray([3]u8, 100);
const Polymer = std.BoundedArray(u8, 1024 * 1024);

test "example" {
    const input = @embedFile("14_example.txt");
    const result = try run(input);
    try std.testing.expectEqual(@as(usize, 1588), result);
}

pub fn main() !void {
    const input = @embedFile("14.txt");
    const result = try run(input);
    std.debug.print("{}\n", .{result});
}

fn run(input: []const u8) !usize {
    const first_newline_idx = std.mem.indexOfScalar(u8, input, '\n') orelse return error.ParseError;

    var rules = try Rules.init(0);
    var lines = std.mem.split(u8, input[first_newline_idx + 2 ..], "\n");
    while (lines.next()) |line| {
        try rules.append(.{ line[0], line[1], line[line.len - 1] });
    }
    var polymer = try Polymer.fromSlice(input[0..first_newline_idx]);

    var i: usize = 0;
    while (i < 10) : (i += 1) {
        try nextStep(&polymer, rules);
    }

    return calculateResult(&polymer);
}

fn findInsertion(rules: Rules, pair: [2]u8) !u8 {
    for (rules.constSlice()) |rule| {
        if (rule[0] == pair[0] and rule[1] == pair[1]) return rule[2];
    } else return error.RuleNotFound;
}

fn nextStep(polymer: *Polymer, rules: Rules) !void {
    var i: usize = 1;

    while (i < polymer.len) : (i += 2) {
        const insertion = try findInsertion(rules, .{ polymer.get(i - 1), polymer.get(i) });
        try polymer.insert(i, insertion);
    }
}

fn calculateResult(polymer: *Polymer) usize {
    var slice = polymer.slice();
    std.sort.sort(u8, slice, {}, comptime std.sort.asc(u8));

    var min = @as(usize, 0) -% 1; // MAXusize
    var max = @as(usize, 0);

    var last = slice[0];
    var running_length: usize = 1;
    for (slice[1..]) |c| {
        if (c == last) {
            running_length += 1;
        } else {
            min = @minimum(running_length, min);
            max = @maximum(running_length, max);

            running_length = 1;
            last = c;
        }
    } else {
        min = @minimum(running_length, min);
        max = @maximum(running_length, max);
    }

    return max - min;
}
