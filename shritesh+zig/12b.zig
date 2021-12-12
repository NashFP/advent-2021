const std = @import("std");

test "examples" {
    const examples = .{
        .{ .input = @embedFile("12_example_1.txt"), .answer = 36 },
        .{ .input = @embedFile("12_example_2.txt"), .answer = 103 },
        .{ .input = @embedFile("12_example_3.txt"), .answer = 3509 },
    };

    inline for (examples) |example| {
        const result = try run(example.input);
        try std.testing.expectEqual(@as(usize, example.answer), result);
    }
}

pub fn main() !void {
    const input = @embedFile("12.txt");
    const result = try run(input);
    std.debug.print("{}\n", .{result});
}

const Map = struct {
    caves: Caves,
    edges: Edges,
    visited: Visited,

    const Caves = std.BoundedArray([]const u8, 64);
    const Visited = [64]usize;
    const Edges = std.BoundedArray([2]usize, 32);

    const start = 0;
    const end = 1;

    fn init() !Map {
        var map = Map{
            .caves = try Caves.fromSlice(&.{ "start", "end" }),
            .edges = try Edges.init(0),
            .visited = undefined,
        };

        return map;
    }

    fn getOrInsertCave(self: *Map, cave: []const u8) !usize {
        for (self.caves.constSlice()) |c, i| {
            if (std.mem.eql(u8, c, cave)) return i;
        } else {
            try self.caves.append(cave);
            return self.caves.len - 1;
        }
    }

    fn insert(self: *Map, from: []const u8, to: []const u8) !void {
        const from_idx = try self.getOrInsertCave(from);
        const to_idx = try self.getOrInsertCave(to);
        try self.edges.append(.{ from_idx, to_idx });
    }

    inline fn isSmall(self: *const Map, idx: usize) bool {
        if (idx == start or idx == end) return true;
        const name = self.caves.get(idx);
        return (name[0] >= 'a' and name[0] <= 'z');
    }

    fn edgesOf(self: Map, needle: usize) EdgeIterator {
        return .{
            .index = 0,
            .needle = needle,
            .edges = self.edges.constSlice(),
        };
    }

    const EdgeIterator = struct {
        index: usize,
        needle: usize,
        edges: []const [2]usize,

        fn next(self: *@This()) ?usize {
            while (self.index < self.edges.len) {
                const current = self.edges[self.index];
                self.index += 1;
                if (current[0] == self.needle) return current[1];
                if (current[1] == self.needle) return current[0];
            } else return null;
        }
    };

    fn countPaths(self: *Map) usize {
        self.visited = std.mem.zeroes(Visited);
        var count: usize = 0;
        self.visit(&count, start);
        return count;
    }

    fn visit(self: *Map, count: *usize, current: usize) void {
        if (current == end) {
            count.* += 1;
            return;
        }

        self.visited[current] += 1;

        var edges = self.edgesOf(current);
        while (edges.next()) |e| {
            if (e == start) continue;
            if (self.visited[e] > 0 and self.isSmall(e) and self.visitedSmallTwice()) continue;
            self.visit(count, e);
        }

        self.visited[current] -= 1;
    }

    fn visitedSmallTwice(self: Map) bool {
        var i: usize = 2;
        while (i < self.caves.len) : (i += 1) {
            if (self.visited[i] > 1 and self.isSmall(i)) return true;
        } else return false;
    }
};

fn run(input: []const u8) !usize {
    var map = try Map.init();

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        const middle = std.mem.indexOfScalar(u8, line, '-').?;
        try map.insert(line[0..middle], line[middle + 1 ..]);
    }

    return map.countPaths();
}
