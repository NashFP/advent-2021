defmodule Day08 do
  @letters_to_number %{
    ~w[a b c e f g] => "0",
    ~w[c f] => "1",
    ~w[a c d e g] => "2",
    ~w[a c d f g] => "3",
    ~w[b c d f] => "4",
    ~w[a b d f g] => "5",
    ~w[a b d e f g] => "6",
    ~w[a c f] => "7",
    ~w[a b c d e f g] => "8",
    ~w[a b c d f g] => "9"
  }

  @length_to_digits %{
    5 => [2,3,5],
    6 => [0,6,9],
    2 => [1],
    7 => [8],
    3 => [7],
    4 => [4]
  }

  def part1(file_name \\ "test2.txt") do
    file_name
    |> parse()
    |> Stream.map(fn [_input, output] ->
      output
      |> Enum.map(fn line ->
        line
        |> String.length()
        |> length_to_digits()
      end)
      |> Enum.count(fn line -> length(line) == 1 end)
    end)
    |> Enum.sum()
  end

  def part2(file_name \\ "test1.txt") do
    file_name
    |> parse()
    |> Stream.map(fn line ->
      [input, output] = line |> Enum.map(fn io -> io |> Enum.map(&String.graphemes/1) end)
      decoder = decoder(input)
      decode_output(output, decoder)
    end)
    |> Enum.sum()
  end

  def decode_output(output, decoder) do
    output
    |> Enum.reduce("", fn numbers, acc ->
      key = numbers |> Enum.map(fn number -> decoder[number] end) |> Enum.sort()
      acc <> Map.get(@letters_to_number, key)
    end)
    |> String.to_integer()
  end

  # You call yourself a programmer with this mess?
  def decoder(input) do
    %{
      [1] => [twos],
      [4] => [fours],
      [7] => [threes],
      [0, 6, 9] => sixes
    } = group_by_lengths(input)

    a = threes -- twos
    b_or_d = fours -- twos

    segments =
      segments()
      |> Map.map(fn {_key, value} -> List.delete(value, hd(a)) end)
      |> Map.put("a", a)
      |> Map.map(fn {_key, value} -> value -- twos end)
      |> Map.put("c", twos)
      |> Map.put("f", twos)
      |> Map.map(fn {_key, value} -> value -- b_or_d end)
      |> Map.put("b", b_or_d)
      |> Map.put("d", b_or_d)

    used_sixes = use_sixes(segments, sixes)
    remove = to_remove(used_sixes)

    segments
    |> Map.merge(used_sixes)
    |> Enum.map(fn
      {key, [value]} -> {value, key}
      {key, [_v1, _v2] = value} -> {hd(value -- remove), key}
    end)
    |> Map.new()
  end

  def group_by_lengths(input) do
    Enum.reduce(input, %{}, fn signal, acc ->
      possible = signal |> length |> length_to_digits()
      Map.update(acc, possible, [signal], & [signal | &1])
    end)
  end

  def use_sixes(segments, sixes) do
    transposed = transpose(segments)

    Enum.map(sixes, fn six ->
      six
        |> Enum.map(fn letter -> {letter, Map.get(transposed, letter)} end)
        |> Enum.reject(fn {_letter, segments} -> segments == ["a"] end)
        |> Enum.reduce(%{}, fn {letter, segments}, acc -> Map.update(acc, segments, [letter], & [letter | &1]) end)
        |> Enum.filter(fn {_segments, letters} -> length(letters) == 1 end)
        |> Enum.reduce(%{}, fn
            {["d", "b"], letter}, acc -> Map.put(acc, "b", letter)
            {["f", "c"], letter}, acc -> Map.put(acc, "f", letter)
            {["g", "e"], letter}, acc -> Map.put(acc, "g", letter)
            _ , acc -> acc
          end)
    end)
    |> Enum.reduce(%{}, fn x, acc -> Map.merge(acc, x) end)
  end

  def to_remove(used_sixes) do
    used_sixes
    |> Enum.map(fn {_key, value} -> value end)
    |> List.flatten()
  end

  def transpose(segments) do
    segments
    |> Enum.reduce(%{}, fn {key,value}, acc -> Enum.reduce(value, acc, fn v, acc2 -> Map.update(acc2, v, [key], & [key | &1]) end) end)
  end

  def segments() do
    letters = ~w(a b c d e f g)
    Enum.reduce(letters, %{}, fn letter, acc -> Map.put(acc, letter, letters) end)
  end

  def length_to_digits(digit_length) do
    Map.get(@length_to_digits, digit_length)
  end

  def parse(file_name) do
    "priv/" <> file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim_trailing()
      |> String.split(" | ")
      |> Enum.map(& String.split(&1, " "))
    end)
  end
end
