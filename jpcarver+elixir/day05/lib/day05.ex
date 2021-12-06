defmodule Day05 do
  def part1(file_name \\ "input.txt"), do:
    run(file_name, &remove_diagonals/1)

  def part2(file_name \\ "input.txt"), do:
    run(file_name, &keep_diagonals/1)

  def run(file_name, handle_diagonals) do
    "priv/" <> file_name
    |> parse()
    |> handle_diagonals.()
    |> lines()
    |> count_overlap_at_least(2)
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
      |> add_vector()
    end)
  end

  def add_vector([[x, start_y], [x, stop_y]] = coord) when start_y < stop_y, do:
    [[0, 1] | coord]

  def add_vector([[x, start_y], [x, stop_y]] = coord) when start_y > stop_y, do:
    [[0, -1] | coord]

  def add_vector([[start_x, y], [stop_x, y]] = coord) when start_x < stop_x, do:
    [[1, 0] | coord]

  def add_vector([[start_x, y], [stop_x, y]] = coord) when start_x > stop_x, do:
    [[-1, 0] | coord]

  def add_vector([[start_x, start_y], [stop_x, stop_y]] = coord) when start_x < stop_x and start_y > stop_y, do:
    [[1, -1] | coord]

  def add_vector([[start_x, start_y], [stop_x, stop_y]] = coord) when start_x > stop_x and start_y > stop_y, do:
    [[-1, -1] | coord]

  def add_vector([[start_x, start_y], [stop_x, stop_y]] = coord) when start_x < stop_x and start_y < stop_y, do:
    [[1, 1] | coord]

  def add_vector([[start_x, start_y], [stop_x, stop_y]] = coord) when start_x > stop_x and start_y < stop_y, do:
    [[-1, 1] | coord]

  def lines(coords), do:
    Enum.reduce(coords, %{}, &line/2)

  def line([vectors, start, stop], acc) do
    line_coords = line_coords(start, stop, vectors)
    add_line(acc, line_coords)
  end

  def line_coords(start, stop, [move_x, move_y]) do
    start
    |> Stream.unfold(fn
      ^stop -> nil
      [x, y] -> {[x, y], [x + move_x, y + move_y]}
    end)
    |> Enum.to_list()
    |> Kernel.++([stop])
    |> Enum.map(&List.to_tuple/1)
  end

  def add_line(grid, line_coords) do
    line_coords
    |> Enum.reduce(grid, fn coord, acc ->
      Map.update(acc, coord, 1, & &1 + 1)
    end)
  end

  def remove_diagonals(coords), do:
    Enum.reject(coords, fn [classification | _line] -> !Enum.any?(classification, fn c -> c == 0 end) end)

  def keep_diagonals(coords), do:
    coords

  def count_overlap_at_least(coords, min) do
    coords
    |> Map.values()
    |> Enum.count(fn value -> value >= min end)
  end
end
