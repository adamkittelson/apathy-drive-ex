defmodule Systems.URL do
  use Systems.Reload

  def random(length \\ 5) do
    :random.seed(:os.timestamp)
    (1..length)
    |> Enum.reduce([], fn(_, codepoints) ->
         [random_alphanumeric | codepoints]
       end)
    |> Enum.join
  end

  defp random_alphanumeric do
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    |> String.codepoints
    |> Enum.shuffle
    |> List.first
  end
end