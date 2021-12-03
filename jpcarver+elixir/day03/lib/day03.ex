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
    |> stream_file()
    |> find_ratings()
    |> life_support()
  end

  def life_support({oxygen, co2}) do
    oxygen * co2
  end

  def find_ratings(stream) do
    oxygen = find_oxygen_generator_rating(stream)
    co2 = find_co2_scrubber_rating(stream)

    {oxygen, co2}
  end

  def find_oxygen_generator_rating(report, index \\ 0)

  def find_oxygen_generator_rating([result], _index) do
    result
    |> Enum.join()
    |> Integer.parse(2)
    |> elem(0)
  end

  def find_oxygen_generator_rating(report, index) do
    most_common = report |> find_min_max(index) |> most_common()

    filtered_report = Enum.filter(report, fn line -> Enum.at(line, index) == most_common end)

    find_oxygen_generator_rating(filtered_report, index + 1)
  end

  def most_common({{_min_bit, min_amt}, {_max_bit, max_amt}}) when min_amt == max_amt do
    "1"
  end

  def most_common({{min_bit, min_amt}, {_max_bit, max_amt}}) when min_amt > max_amt do
    min_bit
  end

  def most_common({{_min_bit, _min_amt}, {max_bit, _max_amt}}) do
    max_bit
  end

  def find_co2_scrubber_rating(report, index \\ 0)

  def find_co2_scrubber_rating([result], _index) do
    result
    |> Enum.join()
    |> Integer.parse(2)
    |> elem(0)
  end

  def find_co2_scrubber_rating(report, index) do
    least_common = report |> find_min_max(index) |> least_common()

    filtered_report = Enum.filter(report, fn line -> Enum.at(line, index) == least_common end)

    find_co2_scrubber_rating(filtered_report, index + 1)
  end

  def least_common({{_min_bit, min_amt}, {_max_bit, max_amt}}) when min_amt == max_amt do
    "0"
  end

  def least_common({{min_bit, min_amt}, {_max_bit, max_amt}}) when min_amt < max_amt do
    min_bit
  end

  def least_common({{_min_bit, _min_amt}, {max_bit, _max_amt}}) do
    max_bit
  end

  def find_min_max(report, index) do
    report
    |> Enum.map(fn line -> Enum.at(line, index) end)
    |> Enum.frequencies()
    |> Enum.min_max_by(fn {_num, count} -> count end)
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

  def min_max(stream) do
    stream
    |> Stream.map(fn list ->
      list
      |> Enum.frequencies()
      |> Enum.min_max_by(fn {_num, count} -> count end)
      |> then(fn {{min, _}, {max, _}} -> {min, max} end)
    end)
  end

  def rates(stream) do
    stream
    |> min_max()
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
