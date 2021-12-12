defmodule Day08.Redo do
  def part1(file_name \\ "test1.txt") do
    file_name
    |> parse()
    |> count_1_4_7_8()
  end

  def part2(file_name \\ "test1.txt") do
    file_name
    |> parse()
    |> sum_output()
  end

  def sum_output(lines) do
    Enum.reduce(lines, 0, fn %{input: input, output: output}, total ->
      output_number = input |> decoder() |> decode(output)
      total + output_number
    end)
  end

  def decode(decoder, output) do
    output
    |> Enum.map_join("", fn number -> decoder[number] end)
    |> String.to_integer()
  end

  def decoder(input) do
    input
    |> by_segment_length()
    |> init_numbers()
    |> handle_lengths_of_six()
    |> handle_lengths_of_five()
    |> invert()
  end

  def init_numbers(groups) do
    Map.new()
    |> Map.put("1", groups[2])
    |> Map.put("4", groups[4])
    |> Map.put("7", groups[3])
    |> Map.put("8", groups[7])
    |> Map.put(:lengths_of_six, groups[6])
    |> Map.put(:lengths_of_five, groups[5])
  end

  def handle_lengths_of_six(numbers) do
    one = numbers["1"]
    four = numbers["4"]
    lengths_of_six = numbers[:lengths_of_six]
    {zero, six, nine} = identify_lengths_of_six(lengths_of_six, four, one)

    numbers
    |> Map.delete(:lengths_of_six)
    |> Map.put("0", zero)
    |> Map.put("6", six)
    |> Map.put("9", nine)
  end

  def handle_lengths_of_five(numbers) do
    one = numbers["1"]
    six = numbers["6"]
    lengths_of_five = numbers[:lengths_of_five]
    {two, three, five} = identify_lengths_of_five(lengths_of_five, six, one)

    numbers
    |> Map.delete(:lengths_of_five)
    |> Map.put("2", two)
    |> Map.put("3", three)
    |> Map.put("5", five)
  end

  def identify_lengths_of_six(lengths_of_six, four, one) do
    {nine, zero_or_six} = identify_by_subset(lengths_of_six, four)
    {zero, six} = identify_by_subset(zero_or_six, one)

    {hd(zero), hd(six), hd(nine)}
  end

  def identify_lengths_of_five(lengths_of_five, six, one) do
    {five, two_or_three} = identify_by_superset(lengths_of_five, six)
    {three, two} = identify_by_subset(two_or_three, one)

    {hd(two), hd(three), hd(five)}
  end

  def identify_by(signals, fun) do
    %{0 => match, 1 => not_match} = Enum.group_by(signals, fun)

    {match, not_match}
  end

  def identify_by_superset(signals, superset) do
    fun = fn maybe -> length(maybe -- superset) end
    identify_by(signals, fun)
  end

  def identify_by_subset(signals, subset) do
    fun = fn maybe -> length(subset -- maybe) end
    identify_by(signals, fun)
  end

  def invert(numbers) do
    numbers
    |> Enum.map(fn {key, value} -> {value, key} end)
    |> Map.new()
  end

  def by_segment_length(input) do
    input
    |> group_by_segment_length()
    |> unwrap_single_length()
  end

  def group_by_segment_length(input) do
    Enum.reduce(input, %{}, fn line, acc ->
      length = length(line)
      Map.update(acc, length, [line], & [line | &1])
    end)
  end

  def unwrap_single_length(groups) do
    Map.map(groups, fn
      {_key, value} when length(value) == 1 -> hd(value)
      {_key, value} -> value
    end)
  end

  def count_1_4_7_8(lines) do
    Enum.reduce(lines, 0, fn %{output: output}, total ->
      total + count_output(output)
    end)
  end

  def count_output(output) do
    Enum.reduce(output, 0, fn number, acc ->
      if length(number) in [2,3,4,7], do: acc + 1, else: acc
    end)
  end

  def parse(file_name) do
    "priv/" <> file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim_trailing()
      |> String.split(" | ")
      |> Enum.map(fn line ->
        line
        |> String.split(" ")
        |> Enum.map(fn text -> text |> String.graphemes() |> Enum.sort() end)
      end)
      |> then(fn [input, output] -> %{input: input, output: output} end)
    end)
    |> Enum.to_list()
  end
end
