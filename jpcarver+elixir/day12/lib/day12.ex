defmodule Day12 do
  def part1(file_name \\ "test1.txt") do
    file_name
    |> graph()
    |> visit("start")
    |> length()
  end

  def part2(file_name \\ "test1.txt") do
    file_name
    |> graph()
    |> visit2("start")
    |> length()
  end

  # I'm not good at graphs
  def visit(graph, vertex, visited \\ [], paths \\ [], fun \\ &visit/5)

  def visit(graph, vertex, visited, paths, fun) do
    if has_visited?(visited, vertex) do
      if large_cave?(vertex) do
        find_paths(graph, vertex, visited, paths, fun)
      else
        paths
      end
    else
      if vertex == "end" do
        [["end" | visited] | paths]
      else
        find_paths(graph, vertex, visited, paths, fun)
      end
    end
  end

  def visit2(graph, vertex, visited \\ [], paths \\ [], fun \\ &visit2/5)

  def visit2(graph, vertex, visited, paths, fun) do
    if has_visited?(visited, vertex) do
      if large_cave?(vertex) do
        find_paths(graph, vertex, visited, paths, fun)
      else
        if vertex == "start" do
          paths
        else
          if any_small_caves_visited_twice?(visited) do
            paths
          else
            find_paths(graph, vertex, visited, paths, fun)
          end
        end
      end
    else
      if vertex == "end" do
        [["end" | visited] | paths]
      else
        find_paths(graph, vertex, visited, paths, fun)
      end
    end
  end

  def any_small_caves_visited_twice?(visited) do
    visited
    |> Enum.reject(fn cave -> cave in ~w[start end] end)
    |> Enum.reject(&large_cave?/1)
    |> Enum.frequencies()
    |> Enum.any?(fn {_cave, count} -> count == 2 end)
  end

  def large_cave?(vertex) do
    vertex == String.upcase(vertex)
  end

  def find_paths(graph, vertex, visited, paths, fun) do
    new_visited = [vertex | visited]
    next = graph[vertex]
    more_paths = Enum.flat_map(next, fn v -> fun.(graph, v, new_visited, paths, fun) end)
    more_paths ++ paths
  end

  def has_visited?(visited, vertex) do
    vertex in visited
  end

  def graph(file_name) do
    "priv/" <> file_name
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim_trailing()
      |> String.split("-")
    end)
    |> Enum.reduce(%{}, &add/2)
  end

  def add(["start", second], acc) do
    Map.update(acc, "start", [second], & [second | &1])
  end

  def add([first, "end"], acc) do
    Map.update(acc, first, ["end"], & ["end" | &1])
  end

  def add([first, second], acc) do
    acc
    |> Map.update(first, [second], & [second | &1])
    |> Map.update(second, [first], & [first | &1])
  end
end
