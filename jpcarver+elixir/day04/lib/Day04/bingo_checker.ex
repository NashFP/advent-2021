defmodule Day04.BingoChecker do
  def is_bingo(board, %{x: column, y: row}) do
    has_vertical = check_bingo(board, fn %{x: x} -> x == column end)
    has_horizontal = check_bingo(board, fn %{y: y} -> y == row end)

    Enum.any?([has_vertical, has_horizontal])
  end

  def check_bingo(board, filter_func) do
    board
    |> Map.values()
    |> Enum.filter(filter_func)
    |> Enum.all?(fn %{is_marked: is_marked} -> is_marked end)
  end
end
