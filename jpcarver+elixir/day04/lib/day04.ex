defmodule Day04 do
  alias Day04.{Game, FirstWinner, LastWinner, Scorer}

  def part1(file_name \\ "input.txt") do
    play(file_name, &FirstWinner.play/1)
  end

  def part2(file_name \\ "input.txt") do
    play(file_name, &LastWinner.play/1)
  end

  def play(file_name, play_func) do
    "priv/" <> file_name
    |> Game.new()
    |> play_func.()
    |> Scorer.score()
  end
end
