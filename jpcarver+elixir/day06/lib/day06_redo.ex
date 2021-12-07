defmodule Day06.Redo do
  def part1(file_name \\ "input.txt") do
    run(file_name, 80)
  end

  def part2(file_name \\ "input.txt") do
    run(file_name, 256)
  end

  def run(file_name, last_day) do
    file_name
    |> parse()
    |> starting_counts()
    |> days(last_day)
    |> Enum.sum()
  end

  def days(counts, last_day) do
    Enum.reduce(1..last_day, counts, &day/2)
  end

  def day(_day, [zero_count | rest]) do
    rest
    |> Kernel.++([zero_count])
    |> List.update_at(6, & &1 + zero_count)
  end

  def starting_counts(fishes) do
    fishes
    |> Enum.frequencies()
    |> Enum.reduce(List.duplicate(0, 9), fn {index, count}, acc ->
      List.update_at(acc, index, & &1 + count)
    end)
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
