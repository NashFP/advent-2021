defmodule Day05 do
  def part1(file_name \\ "input.txt") do
    "priv/" <> file_name
    |> parse()
    |> remove_diagonals()
    |> lines()
    |> count_overlap_at_least(2)
  end

  def part2(file_name \\ "input.txt") do
    "priv/" <> file_name
    |> parse()
    |> lines()
    |> count_overlap_at_least(2)
  end

  def lines(coords) do
    Enum.reduce(coords, %{}, &line/2)
  end

  def remove_diagonals(coords) do
    Enum.reject(coords, fn [classification | _line] -> classification in ~w[northeast northwest southeast southwest]a end)
  end

  def count_overlap_at_least(coords, min) do
    coords
    |> Map.values()
    |> Enum.count(fn value -> value >= min end)
  end

  def line([:vertical, [x, start_y], [x, stop_y]], acc) do
    line_coords = for y <- start_y..stop_y, do: {x, y}
    add_line(acc, line_coords)
  end

  def line([:horizontal, [start_x, y], [stop_x, y]], acc) do
    line_coords = for x <- start_x..stop_x, do: {x, y}
    add_line(acc, line_coords)
  end

  def line([:northeast, start, stop], acc) do
    line_coords = diagonal(start, stop, 1, -1)
    add_line(acc, line_coords)
  end

  def line([:northwest, start, stop], acc) do
    line_coords = diagonal(start, stop, -1, -1)
    add_line(acc, line_coords)
  end

  def line([:southeast, start, stop], acc) do
    line_coords = diagonal(start, stop, 1, 1)
    add_line(acc, line_coords)
  end

  def line([:southwest, start, stop], acc) do
    line_coords = diagonal(start, stop, -1, 1)
    add_line(acc, line_coords)
  end

  def diagonal(start, stop, move_x, move_y) do
    start
    |> Stream.unfold(fn
      ^stop -> nil
      [x, y] -> {[x, y], [x + move_x, y + move_y]}
    end)
    |> Enum.to_list()
    |> Kernel.++([stop])
    |> Enum.map(&List.to_tuple/1)
  end

  def classify([[x, _start_y], [x, _stop_y]] = coord) do
    [:vertical | coord]
  end

  def classify([[_start_x, y], [_stop_x, y]] = coord) do
    [:horizontal | coord]
  end

  def classify([[start_x, start_y], [stop_x, stop_y]] = coord) when start_x < stop_x and start_y > stop_y do
    [:northeast | coord]
  end

  def classify([[start_x, start_y], [stop_x, stop_y]] = coord) when start_x > stop_x and start_y > stop_y do
    [:northwest | coord]
  end

  def classify([[start_x, start_y], [stop_x, stop_y]] = coord) when start_x < stop_x and start_y < stop_y do
    [:southeast | coord]
  end

  def classify([[start_x, start_y], [stop_x, stop_y]] = coord) when start_x > stop_x and start_y < stop_y do
    [:southwest | coord]
  end

  def add_line(grid, line_coords) do
    line_coords
    |> Enum.reduce(grid, fn coord, acc ->
      Map.update(acc, coord, 1, & &1 + 1)
    end)
  end

  def parse(file_name) do
    file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim_trailing()
      |> String.split(" -> ")
      |> Enum.map(fn str ->
        str
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)
      end)
      |> classify()
    end)
  end
end
