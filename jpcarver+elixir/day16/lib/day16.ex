defmodule Day16 do
  import Bitwise

  def part1(file_name \\ "test.txt") do
    file_name
    |> read_file()
    |> run()
    |> sum_versions()
  end

  def part2(file_name \\ "test.txt") do
    file_name
    |> read_file()
    |> run()
    |> evaluate()
  end

  def run(hex) do
    {_rest, packet} =
      hex
      |> Base.decode16!()
      |> create_packet()

    packet
  end

  def create_packet(<<version::3, packet_type_id::3, rest::bits>>) do
    decode_type(%{version: version, packet_type_id: packet_type_id}, rest)
  end

  def decode_type(%{packet_type_id: 4} = partial, rest) do
    {rest, value} = decode_literal(rest, 0)

    {rest, Map.put(partial, :value, value)}
  end

  def decode_type(partial, <<0::1, length::15, subpacket_bits::size(length)-bits, rest::bits>>) do
    subpackets = subpackets_by_length(subpacket_bits, [])

    {rest, Map.put(partial, :subpackets, subpackets)}
  end

  def decode_type(partial, <<1::1, count::11, rest::bits>>) do
    {rest, subpackets} = subpackets_by_count(count, rest, [])

    {rest, Map.put(partial, :subpackets, subpackets)}
  end

  def subpackets_by_count(0, rest, acc) do
    {rest, Enum.reverse(acc)}
  end

  def subpackets_by_count(count, rest, acc) do
    {rest, packet} = create_packet(rest)

    subpackets_by_count(count - 1, rest, [packet | acc])
  end

  def subpackets_by_length(<<>>, acc) do
    Enum.reverse(acc)
  end

  def subpackets_by_length(bits, acc) do
    {rest, packet} = create_packet(bits)

    subpackets_by_length(rest, [packet | acc])
  end

  def decode_literal(<<1::1, parts::4, rest::bits>>, acc) do
    decode_literal(rest, (acc <<< 4) + parts)
  end

  def decode_literal(<<0::1, parts::4, rest::bits>>, acc) do
    {rest, (acc <<< 4) + parts }
  end

  def sum_versions(packet, acc \\ 0)

  def sum_versions(%{subpackets: subpackets, version: version}, acc) do
    sum_subpacket_versions = Enum.reduce(subpackets, 0, &sum_versions/2)
    acc + version + sum_subpacket_versions
  end

  def sum_versions(%{version: version}, acc) do
    acc + version
  end

  def evaluate(%{value: value}) do
    value
  end

  def evaluate(%{packet_type_id: 0, subpackets: subpackets}) do
    subpackets |> Enum.map(&evaluate/1) |> Enum.sum()
  end

  def evaluate(%{packet_type_id: 1, subpackets: subpackets}) do
    subpackets |> Enum.map(&evaluate/1) |> Enum.product()
  end

  def evaluate(%{packet_type_id: 2, subpackets: subpackets}) do
    subpackets |> Enum.map(&evaluate/1) |> Enum.min()
  end

  def evaluate(%{packet_type_id: 3, subpackets: subpackets}) do
    subpackets |> Enum.map(&evaluate/1) |> Enum.max()
  end

  def evaluate(%{packet_type_id: 5, subpackets: [first, second]}) do
    if evaluate(first) > evaluate(second), do: 1, else: 0
  end

  def evaluate(%{packet_type_id: 6, subpackets: [first, second]}) do
    if evaluate(first) < evaluate(second), do: 1, else: 0
  end

  def evaluate(%{packet_type_id: 7, subpackets: [first, second]}) do
    if evaluate(first) == evaluate(second), do: 1, else: 0
  end


  def evaluate(packet) do
    packet
  end

  def read_file(file_name) do
    "priv/" <> file_name
    |> File.read!()
    |> String.trim_trailing()
  end
end
