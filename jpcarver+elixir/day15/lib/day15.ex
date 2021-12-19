defmodule Day15 do
  def part1(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> min_cost()
  end

  def part2(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> add_mins()
    |> add_direction()
    |> add_final_step(24)
    |> expand_grid()
  end

  def add_mins(%{grid: grid, target: {max_x, max_y}}) do
    %{grid: grid, mins: {0, 0}, maxs: {max_x, max_y}}
  end

  def add_direction(state) do
    Map.put(state, :direction, :right)
  end

  def add_final_step(state, final_step) do
    Map.put(state, :final_step, final_step)
  end

  def inc(9), do: 1
  def inc(value), do: value + 1

  def dec(1), do: 9
  def dec(value), do: value - 1

  def opposite_direction(:left), do: :right
  def opposite_direction(:right), do: :left


  def expand_grid(state, step \\ 1)

  def expand_grid(%{grid: grid, maxs: maxs, final_step: final_step}, step) when step > final_step do
    grid
  end

  def expand_grid(%{grid: grid, mins: {min_x, min_y}, maxs: {max_x, max_y}, direction: direction} = state, step) when rem(step, 5) == 0 do
    last_tile = last_tile(state)

    diff = max_y - min_y
    new_grid =
      Enum.reduce(last_tile, grid, fn {{x, y}, value}, acc ->
        Map.put(acc, {x, y + diff + 1}, inc(value))
      end)

    new_min_y = min_y + diff + 1
    new_max_y = new_min_y + diff

    new_state = %{state | grid: new_grid, mins: {min_x, new_min_y}, maxs: {max_x, new_max_y}, direction: opposite_direction(direction)}
    expand_grid(new_state, step + 1)
  end

  def expand_grid(%{grid: grid, mins: {min_x, min_y}, maxs: {max_x, max_y}, direction: :right} = state, step) do
    last_tile = last_tile(state)

    diff = max_x - min_x

    new_grid =
      Enum.reduce(last_tile, grid, fn {{x, y}, value}, acc ->
        Map.put(acc, {x + diff + 1, y}, inc(value))
      end)

    new_min_x = max_x + 1
    new_max_x = new_min_x + diff

    new_state = %{state | grid: new_grid, mins: {new_min_x, min_y}, maxs: {new_max_x, max_y}, direction: :right}
    expand_grid(new_state, step + 1)
  end

  def expand_grid(%{grid: grid, mins: {min_x, min_y}, maxs: {max_x, max_y}, direction: :left} = state, step) do
    last_tile = last_tile(state)

    diff = max_x - min_x

    new_grid =
      Enum.reduce(last_tile, grid, fn {{x, y}, value}, acc ->
        Map.put(acc, {x - diff - 1, y}, dec(value))
      end)

    new_max_x = min_x - 1
    new_min_x = new_max_x - diff

    new_state = %{state | grid: new_grid, mins: {new_min_x, min_y}, maxs: {new_max_x, max_y}, direction: :left}
    expand_grid(new_state, step + 1)
  end


  def last_tile(%{grid: grid, mins: {min_x, min_y}, maxs: {max_x, max_y}}) do
    for y <- min_y..max_y,
        x <- min_x..max_x,
        do: {{x,y}, Map.get(grid, {x, y})},
        into: Map.new()
  end

  def min_cost(%{grid: grid, maxs: {max_x, max_y} = target}) do

    # copy the grid into costs
    # set origin as 0 since the instructions state it is not counted in the cost
    costs = grid |> Map.put({0, 0}, 0)

    # top row except for the first cell
    costs =
      1..max_x
      |> Enum.reduce(costs, fn x, acc ->
        prev_value = Map.get(acc, {x-1, 0})
        Map.update!(acc, {x, 0}, & &1 + prev_value)
      end)

    # first column except for the first cell
    costs =
      1..max_y
      |> Enum.reduce(costs, fn y, acc ->
        prev_value = Map.get(acc, {0, y-1})
        Map.update!(acc, {0, y}, & &1 + prev_value)
      end)

    # the rest of the cells
    costs =
      (for y <- 1..max_y, x <- 1..max_x, do: {x, y})
      |> Enum.reduce(costs, fn {x, y}, acc ->
        top_value = Map.get(acc, {x, y - 1})
        left_value = Map.get(acc, {x - 1, y})
        min = Enum.min([top_value, left_value])
        Map.update!(acc, {x, y}, & &1 + min)
      end)

    Map.get(costs, target)
  end

  def target(grid) do
    grid
    |> Enum.max_by(fn {{x, y}, _value} -> {x, y} end)
    |> elem(0)
  end

  def parse(file_name) do
    list =
      "priv/" <> file_name
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&to_charlist/1)

    grid =
      for {lines, y} <- Enum.with_index(list),
          {value, x} <- Enum.with_index(lines),
          do: {{x, y}, value - ?0},
          into: Map.new()

    %{grid: grid, target: target(grid)}
  end
end
