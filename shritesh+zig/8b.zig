const std = @import("std");

// Depending on the length of the input, we can easily determine
// the digit for some inputs and which segments are active
// Length 2 => 1: c, f
// Length 3 => 7: a, c, f
// Length 4 => 4: b, c, d, f
// Length 7 => 8: a, b, c, d, e, f, g

// For the other two cases, it's trickier but we can deduce based on
// the inputs we always know

// Length 5 => 2, 3 or 5: a, d, g is always active; b, c, e, f differ
// If the segments overlap completely with 1 => 3
// If two segments overlap with 4 => 2
// If three segments overlap with 4 => 5

// Length 6 => 0, 6 or 9: a, b, f, g is always active; c, d, e differ
// If the segments don't overlap completely with 1 => 6
// If it completely overlaps with 4 => 9
// else => 0

test "example" {
    const input = @embedFile("8_example.txt");
    const result = run(input);
    try std.testing.expectEqual(@as(usize, 61229), result);
}

pub fn main() void {
    const input = @embedFile("8.txt");
    const result = run(input);
    std.debug.print("{}\n", .{result});
}

fn run(input: []const u8) usize {
    var total: usize = 0;

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        var pipe_index = std.mem.indexOfScalar(u8, line, '|').?;
        total += process(line[0 .. pipe_index - 1], line[pipe_index + 2 ..]);
    }

    return total;
}

fn process(segments: []const u8, numbers: []const u8) usize {
    var digits: [10]?u7 = .{null} ** 10;
    var remaining: usize = 10;

    while (remaining > 0) {
        var segment_tokens = std.mem.tokenize(u8, segments, " ");
        while (segment_tokens.next()) |segment| {
            var digit = wiresToDigit(segment);
            switch (@popCount(u7, digit)) {
                2 => { // 1
                    if (digits[1] == null) remaining -= 1;
                    digits[1] = digit;
                },
                3 => { // 7
                    if (digits[7] == null) remaining -= 1;
                    digits[7] = digit;
                },
                4 => { // 4
                    if (digits[4] == null) remaining -= 1;
                    digits[4] = digit;
                },
                7 => { // 8
                    if (digits[8] == null) remaining -= 1;
                    digits[8] = digit;
                },
                5 => { // 2,3,5
                    if (digits[1]) |one| {
                        if (digit & one == one) { // 3
                            if (digits[3] == null) remaining -= 1;
                            digits[3] = digit;
                        } else if (digits[4]) |four| {
                            const overlap = @popCount(u7, four & digit);
                            if (overlap == 2) { // 2
                                if (digits[2] == null) remaining -= 1;
                                digits[2] = digit;
                            } else if (overlap == 3) { // 5
                                if (digits[5] == null) remaining -= 1;
                                digits[5] = digit;
                            } else unreachable;
                        }
                    }
                },
                6 => { // 0, 6, 9
                    if (digits[1]) |one| {
                        if (digit & one != one) { // 6
                            if (digits[6] == null) remaining -= 1;
                            digits[6] = digit;
                        } else if (digits[4]) |four| {
                            if (digit & four == four) { // 9
                                if (digits[9] == null) remaining -= 1;
                                digits[9] = digit;
                            } else { // 0
                                if (digits[0] == null) remaining -= 1;
                                digits[0] = digit;
                            }
                        }
                    }
                },
                else => unreachable,
            }
        }
    }

    var number_tokens = std.mem.tokenize(u8, numbers, " ");
    var result: usize = 0;
    while (number_tokens.next()) |number| {
        const n = wiresToDigit(number);
        for (digits) |d, i| {
            if (n == d.?) {
                result = result * 10 + i;
                break;
            }
        } else unreachable;
    }

    return result;
}

fn wiresToDigit(wires: []const u8) u7 {
    var digit: u7 = 0;
    for (wires) |c| {
        if (c < 'a' or c > 'g') unreachable;
        const mask = @as(u7, 1) << @intCast(u3, c - 'a');
        digit |= mask;
    }

    return digit;
}
