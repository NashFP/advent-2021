defmodule Day09 do
  def part1(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> grid()
    |> mark_lowest_points()
    |> sum_risk_level()
  end

  def part2(file_name \\ "test.txt") do
    file_name
  end

  def sum_risk_level(grid) do
    grid
    |> Enum.filter(fn {_key, %{is_lowest: is_lowest}} -> is_lowest == true end)
    |> Enum.map(fn {_key, %{risk_level: risk_level}} -> risk_level end)
    |> Enum.sum()
  end

  def add_risk_level(%{is_lowest: true, elevation: elevation} = location) do
    Map.put(location, :risk_level, elevation + 1)
  end

  def add_risk_level(location) do
    location
  end

  def is_lowest(adjacent_elevations, elevation) do
    elevation < Enum.min(adjacent_elevations)
  end

  def adjacent(grid, {x, y}) do
    [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]
    |> Enum.map(fn {dx, dy} -> Map.get(grid, {x + dx, y + dy}) end)
    |> Enum.reject(&is_nil/1)
    |> Enum.map(fn %{elevation: elevation} -> elevation end)
  end

  def mark_lowest_points(grid) do
    grid
    |> Map.map(fn {coord, %{elevation: elevation} = location} ->
      is_lowest = grid |> adjacent(coord) |> is_lowest(elevation)
      new_location = %{location | is_lowest: is_lowest}
      add_risk_level(new_location)
    end)
  end

  def grid(parsed) do
    for {lines, y} <- Enum.with_index(parsed),
        {elevation, x} <- Enum.with_index(lines),
    do: {{x, y}, %{elevation: elevation, is_lowest: nil}},
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
