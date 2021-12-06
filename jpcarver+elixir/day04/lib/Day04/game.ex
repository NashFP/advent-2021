defmodule Day04.Game do
  alias Day04.Game

  defstruct ~w(numbers boards)a

  def new(file_name) do
    file_name
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> then(fn [numbers_token | board_tokens] ->
       %Game{
          numbers: parse_numbers(numbers_token),
          boards: parse_boards(board_tokens)
        }
    end)
  end

  def parse_numbers(numbers_token) do
    numbers_token
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def parse_boards(board_tokens) do
    Enum.map(board_tokens, fn token ->
      token
      |> String.split("\n", trim: true)
      |> Enum.map(fn b -> String.split(b, " ", trim: true) end)
      |> add_coordinates()
    end)
  end

  def add_coordinates(board) do
    for {lines, y} <- Enum.with_index(board),
        {value, x} <- Enum.with_index(lines),
        do: {String.to_integer(value), %{x: x, y: y, is_marked: false}},
        into: Map.new()
  end
end
