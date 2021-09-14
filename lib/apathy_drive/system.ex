defmodule ApathyDrive.System do
  require Logger

  alias ApathyDrive.{Character, Repo}

  def add_admin(name) do
    case Repo.get_by(Character, name: name) do
      %Character{admin: true} ->
        {:ok, "#{name} is already an admin"}

      %Character{} = character ->
        character
        |> Map.put(:admin, true)
        |> Repo.save!()

        {:ok, "#{name} is now an admin"}

      nil ->
        {:ok, "#{name} does not exist"}
    end
  end

  def remove_admin(name) do
    case Repo.get_by(Character, name: name) do
      %Character{admin: true} = character ->
        character
        |> Map.put(:admin, false)
        |> Repo.save!()

        Logger.info("#{name} is no longer an admin")

      %Character{} ->
        Logger.info("#{name} is not an admin")

      nil ->
        Logger.info("#{name} does not exist")
    end
  end

  def measure(description, function) do
    {time, result} =
      :timer.tc(fn ->
        function.()
      end)

    IO.puts("#{description}: #{div(time, 1000)}")
    result
  end

  def items() do
    require Ecto.Query

    items =
      ApathyDrive.ShopItem
      |> Ecto.Query.preload(:item)
      |> Repo.all()
      |> Enum.map(& &1.item)
      |> Enum.uniq()
      |> Enum.filter(fn item ->
        item.weapon_type == "blade" and item.game_limit == 0
      end)
      |> Enum.map(fn item ->
        %{
          id: item.id,
          name: item.name,
          min: item.min_damage,
          max: item.max_damage,
          speed: item.speed,
          sort: [
            ApathyDrive.Item.cost_in_copper(item),
            (item.min_damage + item.max_damage) / item.speed
          ]
        }
      end)
      |> Enum.sort_by(& &1.sort)

    count = length(items)

    items
    |> Enum.with_index(fn item, index ->
      quality = trunc(index / count * 100) + 3

      IO.puts(
        "#{item.id},#{item.name},#{quality},#{item.speed},#{item.min},#{item.max},#{inspect(item.sort)}"
      )
    end)

    nil
  end
end
