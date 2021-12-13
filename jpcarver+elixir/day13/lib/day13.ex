defmodule Day13 do
  def part1(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> grid()
    |> grab_first_fold()
    |> fold()
    |> count_visible()
  end

  def part2(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> grid()
    |> fold()
    |> prep_for_print()
    |> IO.puts()
  end

  def prep_for_print(%{grid: grid, max: {max_x, max_y}}) do
    0..max_y
    |> Enum.map(fn y ->
      0..max_x
      |> Enum.map(fn x -> Map.get(grid, {x, y}, " ") end)
      |> Enum.join()
    end)
    |> Enum.join("\n")
  end

  def count_visible(%{grid: grid}) do
    Enum.count(grid, fn {{_x, _y}, dot} -> dot == "#" end)
  end

  def fold(%{folds: []} = state) do
    state
  end

  def fold(%{folds: [{"y", fold_y} | rest], grid: grid, max: {max_x, max_y}}) do
    top = top(grid, fold_y)
    bottom = bottom(grid, fold_y) |> flip_bottom(max_y)
    folded_grid = Map.merge(top, bottom)

    fold(%{folds: rest, grid: folded_grid, max: {max_x, max_y - fold_y - 1}})
  end

  def fold(%{folds: [{"x", fold_x} | rest], grid: grid, max: {max_x, max_y}}) do
    left = left(grid, fold_x)
    right = right(grid, fold_x) |> flip_right(max_x)
    folded_grid = Map.merge(left, right)

    fold(%{folds: rest, grid: folded_grid, max: {max_x - fold_x - 1, max_y}})
  end

  def flip_bottom(bottom, max_y) do
    Map.new(bottom, fn {{x, y}, value} -> {{x, max_y - y}, value} end)
  end

  def flip_right(right, max_x) do
    Map.new(right, fn {{x, y}, value} -> {{max_x - x, y}, value} end)
  end

  def top(grid, fold_y) do
    Map.filter(grid, fn {{_x, y}, _value} -> y < fold_y end)
  end

  def bottom(grid, fold_y) do
    Map.filter(grid, fn {{_x, y}, _value} -> y > fold_y end)
  end

  def left(grid, fold_x) do
    Map.filter(grid, fn {{x, _y}, _value} -> x < fold_x end)
  end

  def right(grid, fold_x) do
    Map.filter(grid, fn {{x, _y}, _value} -> x > fold_x end)
  end

  def grab_first_fold(%{folds: [first | _rest]} = state) do
    %{state | folds: [first]}
  end

  def grid(%{coords: coords, folds: folds}) do
    max = max(coords)
    grid = coords |> Enum.reduce(%{}, fn coord, acc -> Map.put(acc, coord, "#") end)

    %{grid: grid, folds: folds, max: max}
  end

  def max(coords) do
    max_x = max_x(coords)
    max_y = max_y(coords)
    {max_x, max_y}
  end

  def max_x(coords) do
    coords
    |> Enum.max_by(fn {x, _y} -> x end)
    |> elem(0)
  end

  def max_y(coords) do
    coords
    |> Enum.max_by(fn {_x, y} -> y end)
    |> elem(1)
  end

  def parse(file_name) do
    "priv/" <> file_name
    |> File.read!()
    |> String.split("\n\n")
    |> then(fn [coords_line, folds_line] ->
      coords = to_coords(coords_line)
      folds = to_folds(folds_line)

      %{coords: coords, folds: folds}
    end)
  end

  def to_folds(line) do
    line
    |> String.split("\n", trim: true)
    |> Enum.map(fn <<"fold along ", axis::binary-size(1), _::binary-size(1), value::binary>> ->
      {axis, String.to_integer(value)}
    end)
  end

  def to_coords(line) do
    line
    |> String.split("\n", trim: true)
    |> Enum.map(fn token ->
      token
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end)
  end
end
