defmodule Day07 do
  def part1(file_name \\ "test.txt") do
    positions = parse(file_name)
    target = median(positions)
    fuel1(positions, target)
  end

  def part2(file_name \\ "test.txt") do
    frequencies = frequencies(file_name)
    position_range = position_range(frequencies)

    fuel2(frequencies, position_range)
  end

  # hat tip to https://github.com/camilleryr/advent-2021/blob/main/elixir/lib/puzzles/day_7.ex
  def fuel2(frequencies, position_range) do
    Enum.reduce_while(position_range, nil, fn target, acc ->
      total =
        frequencies
        |> Enum.map(fn {start, num_crabs} ->
          triangle_distance(start, target) * num_crabs
        end)
        |> Enum.sum()

        if total > acc do
          {:halt, acc}
        else
          {:cont, total}
        end
    end)
  end

  def fuel1(positions, target) do
    Enum.reduce(positions, 0, fn position, acc ->
      acc + abs(position - target)
    end)
  end

  def frequencies(file_name) do
    file_name
    |> parse()
    |> Enum.frequencies()
  end

  def position_range(frequencies) do
    {min, max} = frequencies |> Map.keys() |> Enum.min_max()
    min..max
  end

  def triangle_distance(start, target) do
    Enum.sum(1..abs(start - target))
  end

  def median(positions) do
    Enum.at(positions, div(length(positions), 2))
  end

  def parse(file_name) do
    "priv/" <> file_name
    |> File.read!()
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> Enum.sort()
  end
end
