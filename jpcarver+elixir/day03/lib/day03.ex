defmodule Day03 do
  def part1(file_name \\ "input.txt") do
    "priv/" <> file_name
    |> stream_file()
    |> transpose()
    |> rates()
    |> power_consumption()
  end

  def part2(file_name \\ "input.txt") do
    "priv/" <> file_name
  end

  def power_consumption(rates) do
    rates
    |> Enum.map(fn rate ->
      rate
      |> Enum.join()
      |> Integer.parse(2)
      |> elem(0)
    end)
    |> Enum.product()
  end

  def rates(stream) do
    stream
    |> Stream.map(fn list ->
      list
      |> Enum.frequencies()
      |> Enum.min_max_by(fn {_num, count} -> count end)
      |> then(fn {{epsilon, _}, {gamma, _}} -> {gamma, epsilon} end)
    end)
    |> Enum.unzip()
    |> Tuple.to_list()
  end

  def transpose(stream) do
    stream
    |> Stream.zip()
    |> Stream.map(&Tuple.to_list/1)
  end

  def stream_file(file_name) do
    file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim_trailing()
      |> String.graphemes()
    end)
  end
end
