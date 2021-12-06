defmodule Day04.LastWinner do
  alias Day04.{BingoChecker, Game}

  def play(%Game{numbers: []}) do
    :no_winner
  end

  def play(%Game{numbers: [number | rest], boards: boards}) do
    {number, checked, winners} = check_boards(boards, number)

    case checked do
      [] -> {number, hd(winners)}
      _ -> play(%Game{numbers: rest, boards: checked})
    end
  end

  def check_boards(boards, number, checked \\ [], winners \\ [])

  def check_boards([], number, checked, winners) do
    {number, Enum.reverse(checked), Enum.reverse(winners)}
  end

  def check_boards([board | rest], number, checked, winners) do
    selected_square = Map.get(board, number)

    case selected_square do
      nil -> check_boards(rest, number, [board | checked], winners)
      _ ->
        marked_board = mark(board, number)
        if BingoChecker.is_bingo(marked_board, selected_square) do
          check_boards(rest, number, checked, [marked_board | winners])
        else
          check_boards(rest, number, [marked_board | checked], winners)
        end
    end
  end

  def mark(board, number) do
    Map.update!(board, number, fn square -> %{square | is_marked: true} end)
  end
end
