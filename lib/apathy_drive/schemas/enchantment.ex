defmodule ApathyDrive.Enchantment do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityAttribute,
    AbilityDamageType,
    AbilityTrait,
    Character,
    Enchantment,
    Item,
    ItemInstance,
    Match,
    Mobile,
    Room,
    TimerManager
  }

  schema "enchantments" do
    field(:finished, :boolean, default: false)
    field(:time_elapsed_in_seconds, :integer, default: 0)
    belongs_to(:items_instances, ItemInstance)
    belongs_to(:ability, Ability)
  end

  def tick(%Room{} = room, time, enchanter_ref, %Enchantment{} = enchantment) do
    Room.update_mobile(room, enchanter_ref, fn enchanter ->
      {:ok, enchantment} =
        enchantment
        |> Ecto.Changeset.change(%{
          time_elapsed_in_seconds: enchantment.time_elapsed_in_seconds + time
        })
        |> Repo.update()

      item =
        enchantment
        |> Repo.preload(:items_instances)
        |> Map.get(:items_instances)
        |> Repo.preload(:item)
        |> Item.from_assoc()

      time_left = time_left(enchantment)

      if time_left <= 0 do
        Mobile.send_scroll(enchanter, "<p><span class='cyan'>You finish your work!</span></p>")

        if old_enchantment =
             Repo.get_by(Enchantment, items_instances_id: item.instance_id, finished: true) do
          old_enchantment =
            old_enchantment
            |> Repo.preload(:ability)

          Repo.delete!(old_enchantment)

          Mobile.send_scroll(
            enchanter,
            "<p><span class='dark-yellow'>You've removed #{old_enchantment.ability.name} from #{
              item.name
            }.</span></p>"
          )
        end

        enchantment
        |> Ecto.Changeset.change(%{finished: true})
        |> Repo.update!()

        Mobile.send_scroll(
          enchanter,
          "<p><span class='blue'>You've enchanted #{item.name} with #{enchantment.ability.name}.</span></p>"
        )

        Character.load_items(enchanter)
      else
        Mobile.send_scroll(enchanter, "<p>#{enchantment.ability.traits["TickMessage"]}</p>")

        Mobile.send_scroll(
          enchanter,
          "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
            formatted_time_left(time_left)
          }</span></p>"
        )

        next_tick_time = next_tick_time(enchantment)

        exp = max(100, div(enchanter.max_exp_buffer, 100))

        enchanter =
          enchanter
          |> TimerManager.send_after(
            {{:longterm, enchantment.items_instances_id}, :timer.seconds(next_tick_time),
             {:lt_tick, next_tick_time, enchanter_ref, enchantment}}
          )

        Enum.reduce(enchantment.ability.attributes, enchanter, fn {attribute, _value},
                                                                  enchanter ->
          Character.add_attribute_experience(enchanter, %{
            attribute => 1 / length(Map.keys(enchantment.ability.attributes))
          })
        end)
        |> ApathyDrive.Character.add_experience(exp)
      end
    end)
  end

  def formatted_time_left(seconds) do
    hours = seconds |> div(60) |> div(60)
    minutes = div(seconds, 60) - hours * 60
    seconds = seconds - minutes * 60 - hours * 60 * 60

    [hours, minutes, seconds]
    |> Enum.map(&String.pad_leading(to_string(&1), 2, "0"))
    |> Enum.join(":")
  end

  def time_left(%Enchantment{ability: %Ability{attributes: attributes}} = enchantment) do
    avg_attr_req =
      attributes
      |> Enum.reduce(0, fn {_attr, val}, total -> val + total end)
      |> div(map_size(attributes))

    lt_time = 5 + (avg_attr_req - 50) * 5

    lt_time * 60 - enchantment.time_elapsed_in_seconds
  end

  def next_tick_time(%Enchantment{} = enchantment) do
    min(67, time_left(enchantment))
  end

  def load_enchantment(%Item{instance_id: nil} = item),
    do: Map.put(item, :keywords, Match.keywords(item.name))

  def load_enchantment(%Item{instance_id: id, traits: traits} = item) do
    enchantment =
      __MODULE__
      |> where([ia], ia.items_instances_id == ^id and ia.finished == true)
      |> preload([:ability])
      |> Repo.one()

    if enchantment do
      attributes = AbilityAttribute.load_attributes(enchantment.ability.id)
      ability = Map.put(enchantment.ability, :attributes, attributes)

      ability = put_in(ability.traits, AbilityTrait.load_traits(enchantment.ability.id))

      ability =
        case AbilityDamageType.load_damage(enchantment.ability.id) do
          [] ->
            ability

          damage ->
            update_in(ability.traits, &Map.put(&1, "Damage", damage))
        end

      traits =
        cond do
          ability.kind in ["attack", "curse"] and item.type == "Weapon" ->
            Map.put(traits, "OnHit", ability)

          ability.kind == "blessing" ->
            Map.put(traits, "Passive", ability)

          :else ->
            Map.put(traits, "Grant", ability)
        end

      item
      |> Map.put(:traits, traits)
      |> Map.put(:enchantment_name, ability.name)
      |> Map.put(:keywords, Match.keywords(item.name <> " " <> ability.name))
    else
      item
      |> Map.put(:keywords, Match.keywords(item.name))
    end
  end

  def enchantment_time(%Item{instance_id: nil}), do: 0

  def enchantment_time(%Item{instance_id: id}) do
    __MODULE__
    |> where([e], e.items_instances_id == ^id and e.finished == true)
    |> Repo.all()
    |> Enum.reduce(0, fn %Enchantment{time_elapsed_in_seconds: time}, total ->
      total + time
    end)
  end
end
