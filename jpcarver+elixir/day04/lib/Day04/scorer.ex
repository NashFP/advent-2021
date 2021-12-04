defmodule Day04.Scorer do
  def score({number, board}) do
    sum_unmarked(board) * number
  end

  def sum_unmarked(board) do
    board
    |> Enum.filter(fn {_value, %{is_marked: is_marked}} -> !is_marked end)
    |> Enum.map(fn {value, _square} -> value end)
    |> Enum.sum()
  end
end
