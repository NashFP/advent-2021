defmodule Day11 do
  def part1(file_name \\ "test1.txt") do
    file_name
    |> parse()
    |> grid()
    |> steps(100)
    |> total_flashes()
  end

  def total_flashes(%{flashes: flashes}) do
    flashes
  end

  def steps(grid, target_steps) do
    Enum.reduce(1..target_steps, %{grid: grid, flashes: 0}, fn _step, %{grid: ready_grid, flashes: flashes_so_far} ->
      leveled_up_grid = increase_level(ready_grid)

      %{grid: flashed_grid, flashes: flashes} = flash(leveled_up_grid)

      %{grid: flashed_grid, flashes: flashes_so_far + flashes}
    end)
  end

  # flash states: not_ready, flashable, flashed

  def flash(grid) do
    do_flash(%{grid: grid, flashes: 0})
  end

  def do_flash(%{grid: grid, flashes: total_flashes}) do
    flashable = flashable(grid)

    if Enum.empty?(flashable) do
      new_grid =
        Enum.reduce(grid, %{}, fn
          {coord, %{level: level}}, acc when level > 9 -> Map.put(acc, coord, %{level: 0, state: :not_ready})
          {coord, octopus}, acc -> Map.put(acc, coord, octopus)
        end)

        %{grid: new_grid, flashes: total_flashes}
    else
      flash_grid(grid, flashable)
      |> Map.update!(:flashes, & &1 + total_flashes)
      |> do_flash()
    end
  end

  def flash_grid(grid, flashable) do
    flashes = length(flashable)

    new_grid =
      Enum.reduce(flashable, grid, fn coord, acc ->
        flashed_grid = acc[coord][:state] |> get_and_update_in(& {&1, :flashed}) |> elem(1)

        neighboring_coords = neighboring_coords(flashed_grid, coord)

        Enum.reduce(neighboring_coords, flashed_grid, fn neighboring_coord, flashed_acc ->
          octopus = Map.get(flashed_acc, neighboring_coord) |> Map.update!(:level, & &1 + 1)
          if octopus.level == 10 do
            new_octopus = %{octopus | state: :flashable}
            Map.put(flashed_acc, neighboring_coord, new_octopus)
          else
            Map.put(flashed_acc, neighboring_coord, octopus)
          end
        end)
      end)

    %{grid: new_grid, flashes: flashes}
  end

  def neighboring_coords(grid, {x, y}) do
    Enum.reduce(deltas(), [], fn {dx, dy}, acc ->
      new_coord = {x + dx, y + dy}
      neighbor = Map.get(grid, new_coord)

      case neighbor do
        nil -> acc
        _value -> [new_coord | acc]
      end
    end)
  end

  def deltas() do
    [
      {-1, -1}, {0, -1}, {1, -1},
      {-1,  0},          {1,  0},
      {-1,  1}, {0,  1}, {1,  1}
    ]
  end

  def flashable(grid) do
    grid
    |> Enum.filter(fn {_coord, %{state: state}} -> state == :flashable end)
    |> Enum.map(fn {coord, _state} -> coord end)
  end

  def increase_level(grid) do
    Map.map(grid, fn {_key, %{level: level} = octopus} ->
      new_level = level + 1
      if new_level == 10 do
        %{octopus | level: new_level, state: :flashable}
      else
        %{octopus | level: new_level}
      end
    end)
  end

  def parse(file_name) do
    "priv/" <> file_name
    |> File.read!()
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  def grid(parsed) do
    for {lines, y} <- Enum.with_index(parsed),
        {level, x} <- Enum.with_index(lines),
        do: {{x,y}, %{level: level, state: :not_ready}},
        into: Map.new()
  end
end
