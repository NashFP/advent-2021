defmodule Day06 do
  def part1(file_name \\ "input.txt") do
    file_name
    |> starting_fish()
    |> days(80)
    |> length()
  end

  def part2(file_name \\ "input.txt") do
    file_name
    |> starting_fish()
    |> days(256)
    |> length()
  end

  def days(fishes, last_day) do
    Enum.reduce(1..last_day, fishes, &day/2)
  end

  def day(_day, fishes) do

    acc = %{original: [], new: [], new_count: 0}
    %{original: original, new: new} = Enum.reduce(fishes, acc, &cycle/2)
    Enum.reverse(original) ++ Enum.reverse(new)
  end

  def cycle(0, %{original: original, new: new}) do
    %{original: [6 | original], new: [8 | new]}
  end

  def cycle(fish, %{original: original} = acc) do

    %{acc | original: [fish - 1 | original]}
  end

  def starting_fish(file_name) do
    file_name
    |> parse()
  end

  def parse(file_name) do
    "priv/" <> file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.split(",", trim: true)
      |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.at(0)
  end
end
