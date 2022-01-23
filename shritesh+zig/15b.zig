const std = @import("std");
const max_usize = @as(usize, 0) -% 1; // MAXusize

test "example" {
    const input = @embedFile("15_example.txt");
    var grid = Cave(input).init();
    try std.testing.expectEqual(@as(usize, 315), grid.run());
}

pub fn main() void {
    const input = @embedFile("15.txt");
    var grid = Cave(input).init();
    std.debug.print("{}\n", .{grid.run()});
}

fn Cave(comptime input: []const u8) type {
    const m = comptime std.mem.indexOfScalar(u8, input, '\n') orelse @compileError("invalid input");
    const n = m * 5;

    return struct {
        const Self = @This();
        const Set = std.StaticBitSet(n * n);

        set: Set,
        grid: [n][n]u4,
        distance: [n][n]usize,

        fn nextRisk(risk: u4) u4 {
            if (risk == 9) {
                return 1;
            } else {
                return risk + 1;
            }
        }

        fn init() Self {
            var cave = Self{
                .set = Set.initEmpty(),
                .grid = undefined,
                .distance = undefined,
            };

            var lines = std.mem.split(u8, input, "\n");
            var row: usize = 0;
            while (lines.next()) |line| {
                for (line) |val, col| {
                    var risk = @intCast(u4, val - '0');

                    var y: u4 = 0;
                    var row_risk = risk;
                    while (y < 5) : (y += 1) {
                        var x: u4 = 0;
                        var col_risk = row_risk;
                        while (x < 5) : (x += 1) {
                            cave.grid[y * m + row][x * m + col] = col_risk;
                            cave.distance[y * m + row][x * m + col] = max_usize;
                            cave.set.set((y * m + row) * n + (x * m + col));
                            col_risk = nextRisk(col_risk);
                        }
                        row_risk = nextRisk(row_risk);
                    }
                }
                row += 1;
            }

            cave.distance[0][0] = 0;

            return cave;
        }

        fn minFromSet(self: Self) ?usize {
            var min: ?usize = null;
            var min_val = max_usize;
            var iter = self.set.iterator(.{});
            while (iter.next()) |i| {
                const row = i / n;
                const col = i % n;

                if (self.distance[row][col] < min_val) {
                    min = i;
                    min_val = self.distance[row][col];
                }
            }

            return min;
        }

        fn run(self: *Self) usize {
            while (self.minFromSet()) |u| {
                self.set.unset(u);

                const row = u / n;
                const col = u % n;

                // Up: row - 1, col
                if (row != 0) {
                    const alt = self.distance[row][col] + self.grid[row - 1][col];
                    if (alt < self.distance[row - 1][col]) {
                        self.distance[row - 1][col] = alt;
                    }
                }

                // Right: row, col + 1
                if (col != n - 1) {
                    const alt = self.distance[row][col] + self.grid[row][col + 1];
                    if (alt < self.distance[row][col + 1]) {
                        self.distance[row][col + 1] = alt;
                    }
                }

                // Left: row, col - 1
                if (col != 0) {
                    const alt = self.distance[row][col] + self.grid[row][col - 1];
                    if (alt < self.distance[row][col - 1]) {
                        self.distance[row][col - 1] = alt;
                    }
                }

                // Down: row + 1, col
                if (row != n - 1) {
                    const alt = self.distance[row][col] + self.grid[row + 1][col];
                    if (alt < self.distance[row + 1][col]) {
                        self.distance[row + 1][col] = alt;
                    }
                }
            }

            return self.distance[n - 1][n - 1];
        }
    };
}
