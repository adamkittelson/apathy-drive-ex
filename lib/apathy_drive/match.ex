defmodule ApathyDrive.Match do
  def all(list, match_level, string) do
    case string_number(string) do
      {string, nil} ->
        case match_all(list, match_level, string) do
          [] ->
            nil

          [match] ->
            match

          matches ->
            matches
        end

      {string, number} ->
        one(list, match_level, string, number)
    end
  end

  def match_all(list, :match_name, string) do
    Enum.filter(list, &match_name(string, &1))
  end

  def match_all(list, :match_keyword, string) do
    case match_all(list, :match_name, string) do
      [] -> Enum.filter(list, &match_keyword(string, &1))
      value -> value
    end
  end

  def match_all(list, :name_starts_with, string) do
    case match_all(list, :match_keyword, string) do
      [] -> Enum.filter(list, &name_starts_with(string, &1))
      value -> value
    end
  end

  def match_all(list, :keyword_starts_with, string) do
    case match_all(list, :name_starts_with, string) do
      [] -> Enum.filter(list, &keyword_starts_with(string, &1))
      value -> value
    end
  end

  def match_all(list, :name_contains, string) do
    case match_all(list, :keyword_starts_with, string) do
      [] -> Enum.filter(list, &name_contains(string, &1))
      value -> value
    end
  end

  def one(list, match_level, string) do
    {string, number} = string_number(string)
    one(list, match_level, string, number)
  end

  def one(list, match_level, string, nil) do
    one(list, match_level, string, 0)
  end

  def one(list, match_level, string, number) do
    case all(list, match_level, string) do
      matches when is_list(matches) ->
        Enum.at(matches, number)

      match ->
        match
    end
  end

  def match_name("", _pid), do: false

  def match_name(string, %{name: name}) do
    String.downcase(string) == String.downcase(name)
  end

  def match_keyword("", _pid), do: false

  def match_keyword(string, %{keywords: keywords} = thing) do
    keywords
    |> Enum.any?(fn keyword ->
      String.downcase(string) == String.downcase(keyword)
    end)
  end

  def match_keyword(string, %{name: name} = map) do
    match_keyword(string, Map.put_new(map, :keywords, keywords(name)))
  end

  def keyword_starts_with("", _pid), do: false

  def keyword_starts_with(string, %{keywords: keywords}) do
    keywords
    |> Enum.any?(fn keyword ->
      String.downcase(keyword) |> String.starts_with?(string |> String.downcase())
    end)
  end

  def keyword_starts_with(string, %{name: name} = map) do
    keyword_starts_with(string, Map.put_new(map, :keywords, keywords(name)))
  end

  def name_starts_with("", _pid), do: false

  def name_starts_with(string, %{name: name}) do
    name
    |> String.downcase()
    |> String.starts_with?(string |> String.downcase())
  end

  def name_contains("", _pid), do: false

  def name_contains(string, %{name: name}) do
    name
    |> String.downcase()
    |> String.contains?(string |> String.downcase())
  end

  def string_number(string) do
    number = extract_number(Regex.run(~r/\d+$/, string))
    string = remove_number_from_string(string, number)
    number = indexify_number(number)
    {string, number}
  end

  def extract_number([number]), do: number
  def extract_number(nil), do: nil

  def remove_number_from_string(string, nil), do: string

  def remove_number_from_string(string, number) do
    string
    |> String.replace("#{number}", "")
    |> String.trim_trailing()
  end

  def indexify_number(nil), do: nil

  def indexify_number(number) do
    number
    |> String.to_integer()
    |> Kernel.-(1)
    |> max(0)
  end

  def keywords(name) do
    name
    |> String.downcase()
    |> String.replace(~r/[^a-z ]/, "")
    |> String.split(" ")
  end
end
