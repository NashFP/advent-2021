const std = @import("std");

pub fn main() !void {
    const input = @embedFile("13.txt");
    try run(input);
}

const Paper = struct {
    dots: Set,
    height: usize,
    width: usize,

    const size = 1500;
    const Set = std.StaticBitSet(size * size);

    fn init() Paper {
        return Paper{
            .dots = Set.initEmpty(),
            .height = 0,
            .width = 0,
        };
    }

    fn mark(self: *Paper, x: usize, y: usize) void {
        self.dots.set(y * size + x);
        self.width = @maximum(self.width, x + 1);
        self.height = @maximum(self.height, y + 1);
    }

    fn fold(self: *Paper, axis: u8, n: usize) !void {
        switch (axis) {
            'x' => {
                var dx: usize = 1;
                while (dx <= n) : (dx += 1) {
                    const x = n + dx;
                    const mirror_x = n - dx;
                    var y: usize = 0;
                    while (y < self.height) : (y += 1) {
                        if (self.dots.isSet(y * size + x)) {
                            self.dots.set(y * size + mirror_x);
                        }
                    }
                }

                self.width = n;
            },
            'y' => {
                var dy: usize = 1;
                while (dy <= n) : (dy += 1) {
                    const y = n + dy;
                    const mirror_y = n - dy;
                    var x: usize = 0;
                    while (x < self.width) : (x += 1) {
                        if (self.dots.isSet(y * size + x)) {
                            self.dots.set(mirror_y * size + x);
                        }
                    }
                }

                self.height = n;
            },
            else => return error.InvalidAxisError,
        }
    }

    fn print(self: Paper) void {
        var y: usize = 0;
        while (y < self.height) : (y += 1) {
            var x: usize = 0;
            while (x < self.width) : (x += 1) {
                if (self.dots.isSet(y * size + x)) {
                    std.debug.print("█", .{});
                } else {
                    std.debug.print(" ", .{});
                }
            }

            std.debug.print("\n", .{});
        }
    }
};

fn run(input: []const u8) !void {
    var paper = Paper.init();

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) break;
        const comma_idx = std.mem.indexOfScalar(u8, line, ',') orelse return error.ParseError;
        const x = try std.fmt.parseInt(usize, line[0..comma_idx], 10);
        const y = try std.fmt.parseInt(usize, line[comma_idx + 1 ..], 10);

        paper.mark(x, y);
    }

    while (lines.next()) |line| {
        const equal_idx = std.mem.indexOfScalar(u8, line, '=') orelse return error.ParseError;
        const axis = line[equal_idx - 1];
        const n = try std.fmt.parseInt(usize, line[equal_idx + 1 ..], 10);
        try paper.fold(axis, n);
    }

    paper.print();
}
