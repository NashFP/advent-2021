defmodule Day10 do
  def part1(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> Enum.map(&find_corrupted/1)
    |> Enum.reject(fn line -> line == :not_corrupted end)
    |> Enum.map(fn {_label, token} -> to_points(token) end)
    |> Enum.sum()
  end

  def part2(file_name \\ "test.txt") do
    lines = parse(file_name)
    incomplete_indexes = incomplete_indexes(lines)

    incomplete_indexes
    |> Enum.map(fn index ->
      lines
      |> Enum.at(index)
      |> completion()
      |> points()
      |> score()
    end)
    |> Enum.sort()
    |> then(fn scores ->
      middle_index = div(length(scores), 2)
      Enum.at(scores, middle_index)
    end)
  end

  def score(points) do
    Enum.reduce(points, 0, fn point, acc ->
      (acc * 5) + point
    end)
  end

  def points(opens) do
    Enum.map(opens, fn
      "<" -> 4
      "{" -> 3
      "[" -> 2
      "(" -> 1
    end)
  end

  def completion([head | rest]) do
    completion(%{processed: [head], queue: rest})
  end

  def completion(%{queue: []} = state) do
    state[:processed]
  end

  def completion(%{processed: processed, queue: [current | rest]} = state) do
    if open?(current) do
      completion(%{state | processed: [current | processed], queue: rest})
    else
      completion(%{state | processed: tl(processed), queue: rest})
    end
  end

  def incomplete_indexes(lines) do
    lines
    |> Enum.map(fn line -> line |> find_corrupted() |> status() end)
    |> Enum.with_index()
    |> Enum.filter(fn {status, _index} -> status == :not_corrupted end)
    |> Enum.map(fn {_status, index} -> index end)
  end

  def status(:not_corrupted), do: :not_corrupted
  def status({:corrupted, _token}), do: :corrupted

  def to_points(token) do
    case token do
      ")" -> 3
      "]" -> 57
      "}" -> 1197
      ">" -> 25137
      invalid -> raise "The value #{invalid} is not valid"
    end
  end

  def find_corrupted([head | rest]) do
    find_corrupted(rest, [head])
  end

  def find_corrupted([], _opens) do
    :not_corrupted
  end

  def find_corrupted([current | rest], []) do
    if open?(current) do
      find_corrupted(rest, [current])
    else
      {:corrupted, current}
    end
  end

  def find_corrupted([current | rest], [prev | rest_opens] = opens) do
    if open?(current) do
      find_corrupted(rest, [current | opens])
    else
      if matching?(prev, current) do
        find_corrupted(rest, rest_opens)
      else
        {:corrupted, current}
      end
    end
  end

  def matching?("<", ">"), do: true
  def matching?("{", "}"), do: true
  def matching?("[", "]"), do: true
  def matching?("(", ")"), do: true
  def matching?(_t1, _t2), do: false

  def open?(token) when token in ["<", "{", "[", "("],
    do: true

  def open?(_token),
    do: false

  def parse(file_name) do
    "priv/" <> file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim_trailing()
      |> String.graphemes()
    end)
  end
end
