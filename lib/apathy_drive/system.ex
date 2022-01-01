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

  def damage_types() do
    require Ecto.Query

    types =
      ApathyDrive.AbilityDamageType
      |> Ecto.Query.where(
        [dt],
        dt.damage_type_id == 23 and dt.ability_id not in [1041, 367, 436, 437, 450, 538, 1749] and
          dt.ability_id > 1399
      )
      |> Ecto.Query.preload(:ability)
      |> Repo.all()
      |> Enum.reject(&(is_nil(&1.ability.user_message) or &1.ability.user_message == ""))

    first = List.first(types)

    IO.puts("#{length(types)} - #{first.ability.id} - #{first.ability.user_message}")
  end

  def damage_types(text) do
    require Ecto.Query

    types =
      ApathyDrive.AbilityDamageType
      |> Ecto.Query.where(
        [dt],
        dt.damage_type_id == 23 and dt.ability_id not in [1041, 367, 436, 437, 450, 538, 1749] and
          dt.ability_id > 1399
      )
      |> Ecto.Query.preload(:ability)
      |> Repo.all()
      |> Enum.reject(&(is_nil(&1.ability.user_message) or &1.ability.user_message == ""))
      |> Enum.filter(&String.contains?(&1.ability.user_message, text))

    first = List.first(types)

    if first do
      IO.puts("#{length(types)} - #{first.ability.id} - #{first.ability.user_message}")
    end
  end

  def damage_types(text, type_id) do
    require Ecto.Query

    types =
      ApathyDrive.AbilityDamageType
      |> Ecto.Query.where(
        [dt],
        dt.damage_type_id == 23 and dt.ability_id not in [1041, 367, 436, 437, 450, 538, 1749] and
          dt.ability_id > 1399
      )
      |> Ecto.Query.preload(:ability)
      |> Repo.all()
      |> Enum.reject(&(is_nil(&1.ability.user_message) or &1.ability.user_message == ""))
      |> Enum.filter(&String.contains?(&1.ability.user_message, text))

    IO.puts("#{length(types)} updated")

    types
    |> Enum.each(fn dt ->
      dt
      |> Ecto.Changeset.change(%{damage_type_id: type_id})
      |> Repo.update!()
    end)

    damage_types()
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
