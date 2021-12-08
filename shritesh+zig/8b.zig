const std = @import("std");

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
                    if (digits[1]) |_| continue;
                    digits[1] = digit;
                    remaining -= 1;
                },
                3 => { // 7
                    if (digits[7]) |_| continue;
                    digits[7] = digit;
                    remaining -= 1;
                },
                4 => { // 4
                    if (digits[4]) |_| continue;
                    digits[4] = digit;
                    remaining -= 1;
                },
                7 => { // 8
                    if (digits[8]) |_| continue;
                    digits[8] = digit;
                    remaining -= 1;
                },
                5 => { // 2,3,5
                    if (digits[1]) |one| {
                        if (digit & one == one) { // 3
                            if (digits[3]) |_| continue;
                            digits[3] = digit;
                            remaining -= 1;
                        } else {
                            if (digits[4]) |four| {
                                const overlap = @popCount(u7, four & digit);
                                if (overlap == 2) { // 2
                                    if (digits[2]) |_| continue;
                                    digits[2] = digit;
                                    remaining -= 1;
                                } else if (overlap == 3) { // 5
                                    if (digits[5]) |_| continue;
                                    digits[5] = digit;
                                    remaining -= 1;
                                } else unreachable;
                            }
                        }
                    }
                },
                6 => { // 0, 6, 9
                    if (digits[1]) |one| {
                        if (digit & one != one) { // 6
                            if (digits[6]) |_| continue;
                            digits[6] = digit;
                            remaining -= 1;
                        } else {
                            if (digits[4]) |four| {
                                if (digit & four == four) { // 9
                                    if (digits[9]) |_| continue;
                                    digits[9] = digit;
                                    remaining -= 1;
                                } else { // 0
                                    if (digits[0]) |_| continue;
                                    digits[0] = digit;
                                    remaining -= 1;
                                }
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
            const x = d.?;
            if (n == x) {
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
