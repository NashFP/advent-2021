const std = @import("std");

test "example" {
    const input = @embedFile("14_example.txt");
    const result10 = try run(input, 10);
    const result40 = try run(input, 40);
    try std.testing.expectEqual(@as(usize, 1588), result10);
    try std.testing.expectEqual(@as(usize, 2188189693529), result40);
}

pub fn main() !void {
    const input = @embedFile("14.txt");
    std.debug.print("{}\n", .{try run(input, 10)});
    std.debug.print("{}\n", .{try run(input, 40)});
}

const Polymer = struct {
    rules: [100][3]u8,
    rule_len: usize,

    char_counts: [256]usize, // can be [26]usize or less but I've gots bytes to spare
    rule_counts: [100]usize,

    fn init() Polymer {
        return Polymer{
            .rules = undefined,
            .rule_len = 0,
            .char_counts = .{0} ** 256,
            .rule_counts = .{0} ** 100,
        };
    }

    fn addRule(self: *Polymer, pair: [2]u8, insertion: u8) !void {
        if (self.rule_len == 100) return error.TooManyRulesError;

        self.rules[self.rule_len] = .{ pair[0], pair[1], insertion };
        self.rule_len += 1;
    }

    fn nextStep(self: *Polymer) !void {
        var new_counts: [100]usize = undefined;
        std.mem.copy(usize, &new_counts, &self.rule_counts);

        for (self.rule_counts[0..self.rule_len]) |c, i| {
            if (c == 0) continue;
            const rule = self.rules[i];

            const first_idx = try self.getRuleIdx(.{ rule[0], rule[2] });
            const second_idx = try self.getRuleIdx(.{ rule[2], rule[1] });

            new_counts[i] -= c;
            new_counts[first_idx] += c;
            new_counts[second_idx] += c;

            self.char_counts[rule[2]] += c;
        }

        std.mem.copy(usize, &self.rule_counts, &new_counts);
    }

    fn calculate(self: *Polymer, template: []const u8, steps: usize) !usize {
        for (template) |c|
            self.char_counts[c] += 1;

        for (template[1..]) |c, i| {
            const rule_idx = try self.getRuleIdx(.{ template[i], c });
            self.rule_counts[rule_idx] += 1;
        }

        var step: usize = 0;
        while (step < steps) : (step += 1) {
            try self.nextStep();
        }

        var min = @as(usize, 0) -% 1; // MAXusize
        var max = @as(usize, 0);
        for (self.char_counts) |c| {
            if (c == 0) continue;
            min = @minimum(c, min);
            max = @maximum(c, max);
        }

        return max - min;
    }

    fn getRuleIdx(self: *const Polymer, pair: [2]u8) !usize {
        for (self.rules[0..self.rule_len]) |rule, i| {
            if (rule[0] == pair[0] and rule[1] == pair[1]) return i;
        } else return error.RuleNotFound;
    }
};

fn run(input: []const u8, steps: usize) !usize {
    const first_newline_idx = std.mem.indexOfScalar(u8, input, '\n') orelse return error.ParseError;

    var polymer = Polymer.init();

    var lines = std.mem.split(u8, input[first_newline_idx + 2 ..], "\n");
    while (lines.next()) |line| {
        try polymer.addRule(.{ line[0], line[1] }, line[line.len - 1]);
    }

    return polymer.calculate(input[0..first_newline_idx], steps);
}
