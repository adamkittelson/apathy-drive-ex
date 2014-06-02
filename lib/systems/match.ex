defmodule Systems.Match do
  use Systems.Reload

  def all(pids, :match_name, string) do
    Enum.filter(pids, &(match_name(string, &1)))
  end

  def all(pids, :match_keyword, string) do
    case all(pids, :match_name, string) do
      []    -> Enum.filter(pids, &(match_keyword(string, &1)))
      value -> value
    end
  end

  def all(pids, :name_starts_with, string) do
    case all(pids, :match_keyword, string) do
      []    -> Enum.filter(pids, &(name_starts_with(string, &1)))
      value -> value
    end
  end

  def all(pids, :keyword_starts_with, string) do
    case all(pids, :name_starts_with, string) do
      []    -> Enum.filter(pids, &(keyword_starts_with(string, &1)))
      value -> value
    end
  end

  def all(pids, :name_contains, string) do
    case all(pids, :keyword_starts_with, string) do
      []    -> Enum.filter(pids, &(name_contains(string, &1)))
      value -> value
    end
  end

  def first(pids, :match_name, string) do
    Enum.find(pids, &(match_name(string, &1)))
  end

  def first(pids, :match_keyword, string) do
    case first(pids, :match_name, string) do
      nil   -> Enum.find(pids, &(match_keyword(string, &1)))
      value -> value
    end
  end

  def first(pids, :name_starts_with, string) do
    case first(pids, :match_keyword, string) do
      nil   -> Enum.find(pids, &(name_starts_with(string, &1)))
      value -> value
    end
  end

  def first(pids, :keyword_starts_with, string) do
    case first(pids, :name_starts_with, string) do
      nil   -> Enum.find(pids, &(keyword_starts_with(string, &1)))
      value -> value
    end
  end

  def first(pids, :name_contains, string) do
    case first(pids, :keyword_starts_with, string) do
      nil   -> Enum.find(pids, &(name_contains(string, &1)))
      value -> value
    end
  end

  def match_name("", _pid), do: false
  def match_name(string, pid) do
    String.downcase(string) == pid
                               |> Components.Name.value
                               |> String.downcase
  end

  def match_keyword("", _pid), do: false
  def match_keyword(string, pid) do
    if pid |> Entity.list_components |> Enum.member?(Components.Keywords) do
      pid |> Components.Keywords.value
          |> Enum.any?(fn (keyword) ->
               String.downcase(string) == String.downcase(keyword)
             end)
    else
      false
    end
  end

  def name_starts_with("", _pid), do: false
  def name_starts_with(string, pid) do
    pid |> Components.Name.value
        |> String.downcase
        |> String.starts_with?(string |> String.downcase)
  end

  def keyword_starts_with("", _pid), do: false
  def keyword_starts_with(string, pid) do
    if pid |> Entity.list_components |> Enum.member?(Components.Keywords) do
      pid |> Components.Keywords.value
          |> Enum.any?(fn (keyword) ->
               String.downcase(keyword) |> String.starts_with?(string |> String.downcase)
             end)
    else
      false
    end
  end

  def name_contains("", _pid), do: false
  def name_contains(string, pid) do
    pid |> Components.Name.value
        |> String.downcase
        |> String.contains?(string |> String.downcase)
  end

end
