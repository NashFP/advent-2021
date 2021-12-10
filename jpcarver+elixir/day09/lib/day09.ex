defmodule Day09 do
  def part1(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> grid()
    |> lowest_points()
    |> sum_risk_level()
  end

  def part2(file_name \\ "test.txt") do
    file_name
  end

  def sum_risk_level(lowest) do
    lowest
    |> Enum.map(fn {_key, value} -> value + 1 end)
    |> Enum.sum()
  end

  def is_lowest(adjacent_elevations, elevation) do
    elevation < Enum.min(adjacent_elevations)
  end

  def smallest_adjacent(grid, {x, y}) do
    [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]
    |> Enum.map(fn {dx, dy} -> Map.get(grid, {x + dx, y + dy}) end)
    |> Enum.reject(&is_nil/1)
    |> Enum.min()
  end

  def lowest_points(grid) do
    Map.filter(grid, fn {coord, elevation} ->
      elevation < smallest_adjacent(grid, coord)
    end)
  end

  def grid(parsed) do
    for {lines, y} <- Enum.with_index(parsed),
        {elevation, x} <- Enum.with_index(lines),
        do: {{x, y}, elevation},
        into: Map.new()
  end

  def parse(file_name) do
    "priv/" <> file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim_trailing()
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
    end)
  end
end
