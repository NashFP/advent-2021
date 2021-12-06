defmodule Day04.FirstWinner do
  alias Day04.{BingoChecker, Game}

  def play(%Game{numbers: []}) do
    :no_winner
  end

  def play(%Game{numbers: [number | rest], boards: boards}) do
    {status, result} = check_boards(boards, number)

    case status do
      :no_winner -> play(%Game{numbers: rest, boards: result})
      :winner -> result
    end
  end

  def check_boards(boards, number, checked \\ [])

  def check_boards([], _number, checked) do
    {:no_winner, Enum.reverse(checked)}
  end

  def check_boards([board | rest], number, checked) do
    selected_square = Map.get(board, number)

    case selected_square do
      nil -> check_boards(rest, number, [board | checked])
      _ ->
        marked_board = mark(board, number)
        if BingoChecker.is_bingo(marked_board, selected_square) do
          {:winner, {number, marked_board}}
        else
           check_boards(rest, number, [marked_board | checked])
        end
    end
  end

  def mark(board, number) do
    Map.update!(board, number, fn square -> %{square | is_marked: true} end)
  end
end
