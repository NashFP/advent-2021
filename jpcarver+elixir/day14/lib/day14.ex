defmodule Day14 do
  def part1(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> step(10)
    |> count()
  end

  def part2(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> step(40)
    |> count()
  end

  def count(formula) do
    %{first: first, second: second} =
      formula
        |> Enum.reduce(%{first: %{}, second: %{}}, fn {<<first::binary-size(1), second::binary-size(1)>>, pair_count}, acc ->
          new_first = Map.update(acc[:first], first, pair_count, & &1 + pair_count)
          new_second = Map.update(acc[:second], second, pair_count, & &1 + pair_count)
          %{first: new_first, second: new_second}
        end)

    letter_counts =
      Map.merge(first, second, fn
        _key, count1, count2 when count1 > count2 -> count1
        _key, _count1, count2 -> count2
      end)

    {{_min_letter, min}, {_max_letter, max}} = Enum.min_max_by(letter_counts, fn {_letter, count} -> count end)

    max - min
  end

  def step({formula, _rules}, 0) do
    formula
  end

  def step({formula, rules}, count) do
    new_formula =
      Enum.reduce(formula, %{}, fn {pair, total}, acc ->
        Enum.reduce(rules[pair], acc, fn new_pair, pair_acc ->
          Map.update(pair_acc, new_pair, total, & &1 + total)
        end)
      end)

    step({new_formula, rules}, count - 1)
  end

  def parse(file_name) do
    "priv/" <> file_name
    |> File.read!()
    |> String.split("\n\n")
    |> then(fn [template_text, rules_text] ->
      formula = formula(template_text)
      rules = rules(rules_text)

      {formula, rules}
    end)
  end

  def rules(text) do
    text
    |> String.split("\n")
    |> Enum.reduce(%{}, fn <<key::binary-size(2), " -> ", value::binary>>, acc ->
      [k1, k2] = key |> String.graphemes()
      Map.put(acc, key, [k1 <> value, value <> k2])
    end)
  end

  def formula(text) do
    text
    |> String.graphemes()
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.reduce(%{}, fn pair, acc ->
      Map.update(acc, Enum.join(pair), 1, & &1 + 1)
    end)
  end
end
