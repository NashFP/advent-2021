const std = @import("std");

test "example" {
    const map = try HeightMap.new(@embedFile("9_example.txt"));
    try std.testing.expect(map.riskSum() == 15);
}

pub fn main() !void {
    const map = try HeightMap.new(@embedFile("9.txt"));
    std.debug.print("{}\n", .{map.riskSum()});
}

const HeightMap = struct {
    source: []const u8,
    width: usize,
    height: usize,

    fn new(source: []const u8) !HeightMap {
        const width = std.mem.indexOfScalar(u8, source, '\n') orelse return error.InvalidHeightmap;
        const last_newline = std.mem.lastIndexOfScalar(u8, source, '\n') orelse return error.InvalidHeightmap;
        const height = 1 + last_newline / width;
        return HeightMap{ .source = source, .width = width, .height = height };
    }

    inline fn get(self: HeightMap, row: usize, col: usize) u8 {
        const idx = (self.width + 1) * row + col; // including newlines
        return self.source[idx] - '0';
    }

    fn riskLevel(self: HeightMap, row: usize, col: usize) u8 { // returns 0 if not a low point
        var height = self.get(row, col);

        if (row > 0 and height >= self.get(row - 1, col)) return 0;
        if (row + 1 < self.height and height >= self.get(row + 1, col)) return 0;
        if (col > 0 and height >= self.get(row, col - 1)) return 0;
        if (col + 1 < self.width and height >= self.get(row, col + 1)) return 0;

        return height + 1;
    }

    fn riskSum(self: HeightMap) u16 {
        var sum: u16 = 0;

        var row: usize = 0;
        while (row < self.height) : (row += 1) {
            var col: usize = 0;
            while (col < self.width) : (col += 1) {
                sum += self.riskLevel(row, col);
            }
        }

        return sum;
    }
};
