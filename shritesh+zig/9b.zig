const std = @import("std");
const BitSet = std.StaticBitSet(max_size * max_size);

const max_size = 100;

test "example" {
    const map = try HeightMap.new(@embedFile("9_example.txt"));
    try std.testing.expectEqual(@as(usize, 1134), map.threeLargestBasinProduct());
}

pub fn main() !void {
    const map = try HeightMap.new(@embedFile("9.txt"));
    std.debug.print("{}\n", .{map.threeLargestBasinProduct()});
}

const HeightMap = struct {
    source: []const u8,
    width: usize,
    height: usize,

    fn new(source: []const u8) !HeightMap {
        const width = std.mem.indexOfScalar(u8, source, '\n') orelse return error.InvalidHeightmap;
        const last_newline = std.mem.lastIndexOfScalar(u8, source, '\n') orelse return error.InvalidHeightmap;
        const height = 1 + last_newline / width;
        if (width > max_size or height > max_size) return error.TooBig;
        return HeightMap{ .source = source, .width = width, .height = height };
    }

    inline fn get(self: HeightMap, row: usize, col: usize) u8 {
        const idx = (self.width + 1) * row + col; // including newlines
        return self.source[idx] - '0';
    }

    fn isLowPoint(self: HeightMap, row: usize, col: usize) bool {
        const height = self.get(row, col);
        if (row > 0 and height >= self.get(row - 1, col)) return false;
        if (row + 1 < self.height and height >= self.get(row + 1, col)) return false;
        if (col > 0 and height >= self.get(row, col - 1)) return false;
        if (col + 1 < self.width and height >= self.get(row, col + 1)) return false;

        return true;
    }

    fn setBasinBits(self: HeightMap, bitset: *BitSet, row: usize, col: usize) void {
        const height = self.get(row, col);
        if (height == 9) return;

        bitset.set(row * self.width + col);

        if (row > 0 and self.get(row - 1, col) > height) self.setBasinBits(bitset, row - 1, col);
        if (row + 1 < self.height and self.get(row + 1, col) > height) self.setBasinBits(bitset, row + 1, col);
        if (col > 0 and self.get(row, col - 1) > height) self.setBasinBits(bitset, row, col - 1);
        if (col + 1 < self.width and self.get(row, col + 1) > height) self.setBasinBits(bitset, row, col + 1);
    }

    fn threeLargestBasinProduct(self: HeightMap) usize {
        var basins = [3]usize{ 0, 0, 0 }; // sorted in ascending order after insertion

        var row: usize = 0;
        while (row < self.height) : (row += 1) {
            var col: usize = 0;
            while (col < self.width) : (col += 1) {
                if (self.isLowPoint(row, col)) {
                    var bitset = BitSet.initEmpty();
                    self.setBasinBits(&bitset, row, col);
                    const size = bitset.count();

                    if (size > basins[0]) {
                        basins[0] = size;
                        std.sort.sort(usize, &basins, {}, comptime std.sort.asc(usize));
                    }
                }
            }
        }

        return basins[0] * basins[1] * basins[2];
    }
};
