defmodule ApathyDrive.JSONB do
  def type, do: :jsonb

  def cast(json) when is_binary(json) do
    case Poison.decode(json) do
      {:ok, any} -> {:ok, any}
      _          -> :error
    end
  end
  def cast(%{} = map),               do: {:ok, map}
  def cast(list) when is_list(list), do: {:ok, list}
  def cast(_), do: :error

  def blank?(nil), do: true
  def blank?(_),   do: false

  def load(json) when is_binary(json) do
    case Poison.decode(json) do
      {:ok, any} -> {:ok, any}
      _           -> :error
    end
  end
  def load(nil), do: {:ok, nil}

  def dump(json) when is_binary(json), do: {:ok, json}
  def dump(nil), do: {:ok, nil}
  def dump(any) do
    case Poison.encode(any) do
      {:ok, json} -> {:ok, json}
      _           -> :error
    end
  end

end
