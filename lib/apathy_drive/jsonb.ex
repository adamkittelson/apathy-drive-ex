defmodule ApathyDrive.JSONB do
  @behaviour Ecto.Type

  def type, do: :jsonb

  def cast(json) when is_binary(json) do
    case Poison.decode(json) do
      {:ok, any} -> {:ok, any}
      _          ->
        if String.contains?(json, ["{", "}", "[", "]"]) do
          :error
        else
          {:ok, json}
        end
    end
  end
  def cast(%{} = map),               do: {:ok, map}
  def cast(list) when is_list(list), do: {:ok, list}
  def cast(_), do: :error

  def blank?(nil), do: true
  def blank?(_),   do: false


  def load(""), do: {:ok, nil}
  def load(json) when is_binary(json) do
    case Poison.decode(json) do
      {:ok, any} -> {:ok, any}
      {:error, {:invalid, _, _}} -> {:ok, json}
      {:error, {:invalid, _}} -> {:ok, json}
      {:error, :invalid, _} -> {:ok, json}
      {:error, :invalid} -> {:ok, json}
      _ -> :error
    end
  end
  def load(nil), do: {:ok, nil}
  def load(value), do: {:ok, value}

  def dump(json) when is_binary(json), do: {:ok, json}
  def dump(nil), do: {:ok, nil}
  def dump(any) do
    {:ok, any}
  end

end
