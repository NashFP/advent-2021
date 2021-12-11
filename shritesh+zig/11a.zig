const std = @import("std");

test "example" {
    const input = @embedFile("11_example.txt");
    const result = run(input);
    try std.testing.expectEqual(@as(usize, 1656), result);
}

pub fn main() void {
    const input = @embedFile("11.txt");
    const result = run(input);
    std.debug.print("{}\n", .{result});
}

const Grid = [10][10]u4;
const BitSet = std.StaticBitSet(100);

fn run(input: []const u8) usize {
    var grid = parse(input);

    var flashes: usize = 0;

    var i: usize = 0;
    while (i < 100) : (i += 1) {
        flashes += nextStep(&grid);
    }

    return flashes;
}

// returns the number of flashes
fn nextStep(grid: *Grid) usize {
    // Inc by 1
    for (grid) |*row| {
        for (row.*) |*o| {
            o.* +|= 1;
        }
    }

    var flashes = BitSet.initEmpty();

    // Recursively Flash
    for (grid) |row, r| {
        for (row) |_, c| {
            flash(grid, &flashes, r, c);
        }
    }

    // Reset
    for (grid) |*row, r| {
        for (row.*) |*o, c| {
            if (flashes.isSet(r * 10 + c)) {
                o.* = 0;
            }
        }
    }

    return flashes.count();
}

fn flash(grid: *Grid, flashes: *BitSet, row: usize, col: usize) void {
    const bit_index = row * 10 + col;
    if (flashes.isSet(bit_index)) return;
    if (grid[row][col] <= 9) return;

    // Mark as flashed
    flashes.set(bit_index);

    // Flash Neighbors
    if (row > 0) grid[row - 1][col] +|= 1; // Top
    if (row + 1 < 10) grid[row + 1][col] +|= 1; // Bottom
    if (col > 0) grid[row][col - 1] +|= 1; // Left
    if (col + 1 < 10) grid[row][col + 1] +|= 1; // Right
    if (row > 0 and col > 0) grid[row - 1][col - 1] +|= 1; // Top Left
    if (row > 0 and col + 1 < 10) grid[row - 1][col + 1] +|= 1; // Top Right
    if (row + 1 < 10 and col > 0) grid[row + 1][col - 1] +|= 1; // Bottom Left
    if (row + 1 < 10 and col + 1 < 10) grid[row + 1][col + 1] +|= 1; // Bottom Right

    // Make neigbors flash
    if (row > 0) flash(grid, flashes, row - 1, col); // Top
    if (row + 1 < 10) flash(grid, flashes, row + 1, col); // Bottom
    if (col > 0) flash(grid, flashes, row, col - 1); // Left
    if (col + 1 < 10) flash(grid, flashes, row, col + 1); // Right
    if (row > 0 and col > 0) flash(grid, flashes, row - 1, col - 1); // Top Left
    if (row > 0 and col + 1 < 10) flash(grid, flashes, row - 1, col + 1); // Top Right
    if (row + 1 < 10 and col > 0) flash(grid, flashes, row + 1, col - 1); // Bottom Left
    if (row + 1 < 10 and col + 1 < 10) flash(grid, flashes, row + 1, col + 1); // Bottom Right
}

fn parse(input: []const u8) Grid {
    var grid: Grid = undefined;

    var row: usize = 0;
    var col: usize = 0;

    for (input) |c| {
        if (c == '\n') {
            col = 0;
            row += 1;
            continue;
        }

        grid[row][col] = @intCast(u4, c - '0');
        col += 1;
    }

    return grid;
}
