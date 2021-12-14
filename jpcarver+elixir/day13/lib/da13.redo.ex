defmodule Day13.Redo do
  def part1(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> grab_first_fold()
    |> fold()
    |> MapSet.size()
  end

  def part2(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> fold()
    |> print_format()
    |> IO.puts()
  end

  def print_format(coords) do
    max_x = coords |> Enum.max_by(fn {x, _y} -> x end) |> elem(0)
    max_y = coords |> Enum.max_by(fn {_x, y} -> y end) |> elem(1)

    0..max_y
    |> Enum.map_join("\n", fn y ->
      0..max_x
      |> Enum.map_join("", fn x ->
        if MapSet.member?(coords, {x, y}), do: "#", else: " "
      end)
    end)
  end

  def fold(%{coords: coords, folds: []}) do
    coords
  end

  def fold(%{coords: coords, folds: [{"y", fold_y} | rest]}) do
    new_coords = fold_to_top(coords, fold_y)

    fold(%{coords: new_coords, folds: rest})
  end

  def fold(%{coords: coords, folds: [{"x", fold_x} | rest]}) do
    new_coords = fold_to_left(coords, fold_x)

    fold(%{coords: new_coords, folds: rest})
  end

  def fold_to_top(coords, fold_y) do
    Enum.reduce(coords, MapSet.new, fn
      {x, y}, acc when y > fold_y -> MapSet.put(acc, {x, y - (2 * (y - fold_y))})
      coord, acc -> MapSet.put(acc, coord)
    end)
  end

  def fold_to_left(coords, fold_x) do
    Enum.reduce(coords, MapSet.new, fn
      {x, y}, acc when x > fold_x -> MapSet.put(acc, {x - (2 * (x - fold_x)), y})
      coord, acc -> MapSet.put(acc, coord)
    end)
  end

  def grab_first_fold(%{folds: [first | _rest]} = state) do
    %{state | folds: [first]}
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
    |> MapSet.new()
  end
end
