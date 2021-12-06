const std = @import("std");

test "example" {
    const input = @embedFile("5_example.txt");
    const result = try run(input);
    try std.testing.expectEqual(@as(u32, 12), result);
}

pub fn main() !void {
    const input = @embedFile("5.txt");
    const result = try run(input);
    std.debug.print("{}\n", .{result});
}

const Line = struct {
    from_x: u16,
    from_y: u16,
    to_x: u16,
    to_y: u16,

    fn parse(input: []const u8) !Line {
        var nums = std.mem.tokenize(u8, input, " ,->");
        var fx = nums.next() orelse return error.ReadError;
        var fy = nums.next() orelse return error.ReadError;
        var tx = nums.next() orelse return error.ReadError;
        var ty = nums.next() orelse return error.ReadError;

        return Line{
            .from_x = try std.fmt.parseInt(u16, fx, 10),
            .from_y = try std.fmt.parseInt(u16, fy, 10),
            .to_x = try std.fmt.parseInt(u16, tx, 10),
            .to_y = try std.fmt.parseInt(u16, ty, 10),
        };
    }
};

fn run(input: []const u8) !u32 {
    var grid: [1000][1000]u2 = .{.{0} ** 1000} ** 1000;

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |l| {
        const line = try Line.parse(l);

        var x = line.from_x;
        var y = line.from_y;
        grid[x][y] +|= 1;

        while (!(x == line.to_x and y == line.to_y)) {
            if (x < line.to_x) x += 1 else if (x > line.to_x) x -= 1;
            if (y < line.to_y) y += 1 else if (y > line.to_y) y -= 1;
            grid[x][y] +|= 1;
        }
    }

    var count: u32 = 0;
    for (grid) |row| {
        for (row) |n| {
            if (n >= 2) count += 1;
        }
    }

    return count;
}
