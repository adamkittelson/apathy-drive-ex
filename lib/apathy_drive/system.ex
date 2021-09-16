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
      |> Ecto.Query.where([si], not is_nil(si.restock_frequency_in_minutes))
      |> Ecto.Query.preload(:item)
      |> Repo.all()
      |> Enum.map(& &1.item)
      |> Enum.uniq()
      |> Enum.filter(fn item ->
        item.worn_on == "Wrist" and item.game_limit == 0
      end)
      |> Enum.reject(
        &(&1.name in [
            "wooden hammer",
            "lockpicks",
            "thief's kit",
            "pig on a spit",
            "gold jeweled ring",
            "smoky black talisman"
          ])
      )
      |> Enum.map(fn item ->
        defense = ApathyDrive.ItemTrait.load_traits(item.id)["Defense"]

        %{
          id: item.id,
          name: item.name,
          defense: defense,
          cost: ApathyDrive.Item.cost_in_copper(item),
          sort: [
            ApathyDrive.Item.cost_in_copper(item),
            defense
          ]
        }
      end)
      |> Enum.group_by(& &1.sort)
      |> Enum.sort()

    count = length(items)

    items
    |> Enum.with_index(fn {_sort, items}, index ->
      Enum.each(items, fn item ->
        quality = trunc(index / count * 100) + 3

        IO.puts("#{item.id},#{item.name},#{quality},#{item.defense},#{item.cost}")
      end)
    end)

    nil
  end
end
