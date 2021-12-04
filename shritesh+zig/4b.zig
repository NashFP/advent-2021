const std = @import("std");

test "example" {
    const data = @embedFile("4_example.txt");
    const result = try run(data);
    try std.testing.expectEqual(@as(u32, 1924), result);
}

pub fn main() !void {
    const data = @embedFile("4.txt");
    const result = try run(data);
    std.debug.print("{}\n", .{result});
}

fn eventualWinnerScore(bingos: []Bingo, numbers: []u8) !u32 {
    var remaining: usize = bingos.len;

    for (numbers) |n| {
        for (bingos) |*b| {
            if (b.hasWon()) continue;
            if (b.mark(n) and b.hasWon()) {
                remaining -= 1;
                if (remaining == 0) {
                    return n * b.countUnmarked();
                }
            }
        }
    } else return error.NoEventualWinnerError;
}

const Bingo = struct {
    numbers: [25]u8 = undefined,
    marked: u25 = 0,

    fn mark(self: *Bingo, number: u8) bool {
        for (self.numbers) |n, i| {
            if (n == number) {
                self.marked |= @as(u25, 1) << @intCast(u5, i);
                return true;
            }
        } else return false;
    }

    inline fn isMarked(self: Bingo, n: usize) bool {
        const mask = @as(u25, 1) << @intCast(u5, n);
        return self.marked & mask != 0;
    }

    fn hasWon(self: Bingo) bool {
        var rowMask = @as(u25, 0b11111);
        var colMask = @as(u25, 0b00001_00001_00001_00001_00001);
        var i: usize = 0;
        while (i < 5) : (i += 1) {
            if (self.marked & rowMask == rowMask) return true;
            if (self.marked & colMask == colMask) return true;
            rowMask <<= 5;
            colMask <<= 1;
        } else return false;
    }

    fn countUnmarked(self: Bingo) u16 {
        var total: u16 = 0;

        for (self.numbers) |n, i| {
            if (!self.isMarked(i)) total += n;
        }

        return total;
    }

    fn debug(self: Bingo) void {
        std.debug.print("== {} {}\n", .{ self.hasWon(), self.countUnmarked() });

        for (self.numbers) |n, i| {
            if (i % 5 == 0) std.debug.print("\n", .{});

            if (self.isMarked(i)) {
                std.debug.print("\t\x1B[4m{}\x1B[0m", .{n}); // ANSI underline
            } else std.debug.print("\t{}", .{n});
        }

        std.debug.print("==\n", .{});
    }
};

fn run(input: []const u8) !u32 {
    var numbers: [100]u8 = undefined;
    var count: usize = 0;

    const newline = std.mem.indexOfScalar(u8, input, '\n') orelse return error.NumbersReadError;

    var ns = std.mem.split(u8, input[0..newline], ",");
    while (ns.next()) |n| {
        numbers[count] = try std.fmt.parseInt(u8, n, 10);
        count += 1;
    }

    var bingos: [100]Bingo = .{Bingo{}} ** 100;
    var bingo_count: usize = 0;

    var tokens = std.mem.tokenize(u8, input[newline..], "\n ");
    var number_count: usize = 0;
    while (tokens.next()) |n| {
        bingos[bingo_count].numbers[number_count] = try std.fmt.parseInt(u8, n, 10);
        number_count += 1;

        if (number_count == 25) {
            bingo_count += 1;
            number_count = 0;
        }
    } else if (number_count != 0) return error.IncompleteBoardError;

    return eventualWinnerScore(bingos[0..bingo_count], numbers[0..count]);
}
