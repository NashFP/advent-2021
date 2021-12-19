defmodule Day15.Redo do
  def part1(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> dijkstra()
  end

  def part2(file_name \\ "test.txt") do
    file_name
    |> parse()
    |> add_mins()
    |> add_direction()
    |> add_final_step(24)
    |> expand_grid()
    |> dijkstra()
  end

  def pop(queue) do
    element =
      queue
      |> MapSet.to_list()
      |> Enum.min_by(fn {distance, _point} -> distance end)

    {element |> elem(1), MapSet.delete(queue, element)}
  end

  def dijkstra(%{grid: grid, costs: costs, queue: queue, visited: visited, target: target} = state) do

    queue_empty? = MapSet.equal?(queue, MapSet.new())

    if queue_empty? do
      costs[target]
    else
      {current_point, queue} = pop(queue)
      new_visited = MapSet.put(visited, current_point)

      points_with_distances =
        current_point
        |> neighbors()
        |> Enum.reduce(%{}, fn neighbor, acc ->
          distance = Map.get(grid, neighbor)
          if is_nil(distance) do
            acc
          else
            Map.put(acc, neighbor, distance)
          end
        end)

       {new_queue, new_costs} =
        Enum.reduce(points_with_distances, {queue, costs}, fn {neighbor, distance}, {acc_queue, acc_costs} ->
          if MapSet.member?(new_visited, neighbor) do
            {acc_queue, acc_costs}
          else
            old_cost = costs[neighbor]
            new_cost = costs[current_point] + distance
            if new_cost < old_cost do
              {MapSet.put(acc_queue, {new_cost, neighbor}), Map.put(acc_costs, neighbor, new_cost)}
            else
              {acc_queue, acc_costs}
            end
          end
        end)

        %{grid: grid, costs: new_costs, queue: new_queue, visited: new_visited}


        dijkstra(%{state | grid: grid, costs: new_costs, queue: new_queue, visited: new_visited})

    end
  end

  def dijkstra(%{grid: grid, target: target}) do
    origin = {0, 0}

    costs = grid |> Map.keys |> Enum.reduce(%{}, fn key, acc -> Map.put(acc, key, :infinity) end) |> Map.put(origin, 0)
    visited = MapSet.new([origin])
    queue = MapSet.new([{0, origin}])

    dijkstra(%{grid: grid, costs: costs, queue: queue, visited: visited, target: target})
  end

  def neighbors({x, y}) do
    [{x, y - 1}, {x + 1, y}, {x, y + 1}, {x - 1, y}]
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
    %{grid: grid, target: maxs}
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
