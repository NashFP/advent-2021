const std = @import("std");

test "example" {
    const input = @embedFile("11_example.txt");
    const result = run(input);
    try std.testing.expectEqual(@as(usize, 195), result);
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

    var i: usize = 1;
    while (true) : (i += 1) {
        if (nextStep(&grid) == 100) return i;
    }
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

    const has_top = row > 0;
    const has_bottom = row + 1 < 10;
    const has_left = col > 0;
    const has_right = col + 1 < 10;

    // Flash Neighbors
    if (has_top) grid[row - 1][col] +|= 1; // Top
    if (has_bottom) grid[row + 1][col] +|= 1; // Bottom
    if (has_left) grid[row][col - 1] +|= 1; // Left
    if (has_right) grid[row][col + 1] +|= 1; // Right
    if (has_top and has_left) grid[row - 1][col - 1] +|= 1; // Top Left
    if (has_top and has_right) grid[row - 1][col + 1] +|= 1; // Top Right
    if (has_bottom and has_left) grid[row + 1][col - 1] +|= 1; // Bottom Left
    if (has_bottom and has_right) grid[row + 1][col + 1] +|= 1; // Bottom Right

    // Make neigbors flash
    if (has_top) flash(grid, flashes, row - 1, col); // Top
    if (has_bottom) flash(grid, flashes, row + 1, col); // Bottom
    if (has_left) flash(grid, flashes, row, col - 1); // Left
    if (has_right) flash(grid, flashes, row, col + 1); // Right
    if (has_top and has_left) flash(grid, flashes, row - 1, col - 1); // Top Left
    if (has_top and has_right) flash(grid, flashes, row - 1, col + 1); // Top Right
    if (has_bottom and has_left) flash(grid, flashes, row + 1, col - 1); // Bottom Left
    if (has_bottom and has_right) flash(grid, flashes, row + 1, col + 1); // Bottom Right
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
