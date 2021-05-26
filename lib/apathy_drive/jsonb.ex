defmodule ApathyDrive.JSONB do
  use Ecto.Type

  def type, do: :jsonb

  def cast(json) when is_binary(json) do
    case Jason.decode(json) do
      {:ok, any} ->
        {:ok, any}

      _ ->
        {:ok, json}
    end
  end

  def cast(%{} = map), do: {:ok, map}
  def cast(list) when is_list(list), do: {:ok, list}
  def cast(number) when is_number(number), do: {:ok, number}
  def cast(boolean) when is_boolean(boolean), do: {:ok, boolean}
  def cast(_), do: :error

  def blank?(nil), do: true
  def blank?(_), do: false

  def load(""), do: {:ok, nil}

  def load(json) when is_binary(json) do
    case Jason.decode(json) do
      {:ok, any} ->
        {:ok, any}

      {:error, %Jason.DecodeError{data: data}} ->
        {:ok, data}

      other ->
        IO.inspect(other)
        :error
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
