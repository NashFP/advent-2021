const std = @import("std");

test "example" {
    const data = @embedFile("4_example.txt");
    const result = try run(data);
    try std.testing.expectEqual(@as(u32, 4512), result);
}

pub fn main() !void {
    const data = @embedFile("4.txt");
    const result = try run(data);
    std.debug.print("{}\n", .{result});
}

const Bingo = struct {
    numbers: [25]u8 = undefined,
    rowMarked: u25 = 0, // row-major bitset
    colMarked: u25 = 0, // col-major bitset, makes hasWon easier

    fn mark(self: *Bingo, number: u8) bool {
        for (self.numbers) |n, i| {
            if (n == number) {
                self.rowMarked |= @as(u25, 1) << @intCast(u5, i);
                self.colMarked |= @as(u25, 1) << @intCast(u5, 5 * (i % 5) + (i / 5)); // Column transpose
                return true;
            }
        } else return false;
    }

    inline fn isRowMarked(self: Bingo, n: usize) bool {
        const mask = @as(u25, 1) << @intCast(u5, n);
        return self.rowMarked & mask != 0;
    }

    fn hasWon(self: Bingo) bool {
        var mask = @as(u25, 0b11111);
        var i: usize = 0;
        while (i < 5) : (i += 1) {
            if (self.rowMarked & mask == mask) return true;
            if (self.colMarked & mask == mask) return true;
            mask <<= 5;
        } else return false;
    }

    fn countUnmarked(self: Bingo) u16 {
        var total: u16 = 0;

        for (self.numbers) |n, i| {
            if (!self.isRowMarked(i)) total += n;
        }

        return total;
    }

    fn debug(self: Bingo) void {
        std.debug.print("== {} {}\n", .{ self.hasWon(), self.countUnmarked() });

        for (self.numbers) |n, i| {
            if (i % 5 == 0) std.debug.print("\n", .{});

            if (self.isRowMarked(i)) {
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

    for (numbers[0..count]) |n| {
        for (bingos[0..bingo_count]) |*b| {
            if (b.mark(n) and b.hasWon()) return b.countUnmarked() * n;
        }
    } else return error.NoWinnerError;
}
