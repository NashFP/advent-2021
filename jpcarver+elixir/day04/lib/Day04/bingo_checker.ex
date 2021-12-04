defmodule Day04.BingoChecker do
  def is_bingo(board, %{x: x, y: y}) do
    has_vertical = check_bingo_vertical(board, x)
    has_horizontal = check_bingo_horizontal(board, y)

    Enum.any?([has_vertical, has_horizontal])
  end

  def check_bingo_vertical(board, column) do
    board
    |> Map.values()
    |> Enum.filter(fn %{x: x} -> x == column end)
    |> Enum.all?(fn %{is_marked: is_marked} -> is_marked end)
  end

  def check_bingo_horizontal(board, row) do
    board
    |> Map.values()
    |> Enum.filter(fn %{y: y} -> y == row end)
    |> Enum.all?(fn %{is_marked: is_marked} -> is_marked end)
  end
end
