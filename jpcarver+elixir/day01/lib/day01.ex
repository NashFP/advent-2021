defmodule Day01 do
  def part1() do
    "priv/input.txt"
    |> stream_file()
    |> Enum.reduce(%{prev: nil, count: 0}, &count_increases/2)
    |> Map.get(:count)
  end

  def part2() do
    "priv/input.txt"
    |> stream_file()
    |> Stream.chunk_every(3, 1, :discard)
    |> Stream.map(&Enum.sum/1)
    |> Enum.reduce(%{prev: nil, count: 0}, &count_increases/2)
    |> Map.get(:count)
  end

  def count_increases(depth, %{prev: nil, count: 0}) do
    %{prev: depth, count: 0}
  end

  def count_increases(depth, %{prev: prev, count: count}) when depth > prev do
    %{prev: depth, count: count + 1}
  end

  def count_increases(depth, acc) do
    %{acc | prev: depth}
  end

  def stream_file(file_name) do
    file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim_trailing()
      |> String.to_integer()
    end)
  end
end
