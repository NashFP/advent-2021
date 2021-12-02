defmodule Day02 do
  def part1(file_name \\ "input.txt") do
    run(file_name, %{x: 0, y: 0})
  end

  def part2(file_name \\ "input.txt") do
    run(file_name, %{x: 0, y: 0, aim: 0})
  end

  def run(file_name, acc) do
    "priv/" <> file_name
    |> stream_file()
    |> Enum.reduce(acc, &move/2)
    |> then(fn %{x: x, y: y} -> x * y end)
  end

  def move({"forward", amount}, %{x: x, y: y, aim: aim}) do
    %{x: x + amount, y: y + (amount * aim), aim: aim}
  end

  def move({"down", amount}, %{aim: aim} = acc) do
    %{acc | aim: aim + amount}
  end

  def move({"up", amount}, %{aim: aim} = acc) do
    %{acc | aim: aim - amount}
  end

  def move({"forward", amount}, %{x: x} = acc) do
    %{acc | x: x + amount}
  end

  def move({"down", amount}, %{y: y} = acc) do
    %{acc | y: y + amount}
  end

  def move({"up", amount}, %{y: y} = acc) do
    %{acc | y: y - amount}
  end

  def stream_file(file_name) do
    file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim()
      |> String.split(" ")
      |> then(fn [cmd, amount] -> {cmd, String.to_integer(amount)} end)
    end)
  end
end
