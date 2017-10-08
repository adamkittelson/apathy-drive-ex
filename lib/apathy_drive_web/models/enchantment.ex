defmodule ApathyDrive.Enchantment do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, Enchantment, Item, ItemInstance, Mobile, Room, TimerManager}

  schema "enchantments" do
    field :finished, :boolean, default: false
    field :time_elapsed_in_seconds, :integer, default: 0
    belongs_to :items_instances, ItemInstance
    belongs_to :ability, Ability
  end

  def tick(%Room{} = room, time, enchanter_ref, %Enchantment{} = enchantment) do
    Room.update_mobile(room, enchanter_ref, fn enchanter ->
      {:ok, enchantment} =
        enchantment
        |> Ecto.Changeset.change(%{time_elapsed_in_seconds: enchantment.time_elapsed_in_seconds + time})
        |> Repo.update

      time_left = time_left(enchantment)

      if time_left <= 0 do
        enchantment
        |> Ecto.Changeset.change(%{finished: true})
        |> Repo.update!
        Mobile.send_scroll(enchanter, "<p><span class='cyan'>You finish your work!</span></p>")

        item =
          Enum.find(enchanter.equipment, &(&1.instance_id == enchantment.items_instances_id)) ||
          Enum.find(enchanter.inventory, &(&1.instance_id == enchantment.items_instances_id))

        enchanted_item = Ability.apply_item_enchantment(item, enchantment.ability)

        Ability.display_cast_message(room, enchanter, item, enchantment.ability)

        cond do
          item in enchanter.equipment ->
            update_in(enchanter.equipment, fn equipment ->
              equipment
              |> List.delete(item)
              |> List.insert_at(0, enchanted_item)
            end)
          item in enchanter.inventory ->
            update_in(enchanter.inventory, fn inventory ->
              inventory
              |> List.delete(item)
              |> List.insert_at(0, enchanted_item)
            end)
        end
      else
        Mobile.send_scroll(enchanter, "<p>#{enchantment.ability.traits["TickMessage"]}</p>")
        Mobile.send_scroll(enchanter, "<p>Time Left: #{formatted_time_left(time_left)}</p>")
        next_tick_time = next_tick_time(enchantment)

        exp =
          [:strength, :agility, :intellect, :willpower, :health, :charm]
          |> Enum.map(&Mobile.attribute_at_level(enchanter, &1, enchanter.level))
          |> Enum.reduce(0, &(&1 + &2))
          |> trunc

        enchanter
        |> TimerManager.send_after({{:longterm, enchantment.items_instances_id}, :timer.seconds(next_tick_time), {:lt_tick, next_tick_time, enchanter_ref, enchantment}})
        |> ApathyDrive.Character.add_experience(exp)
      end
    end)
  end

  def formatted_time_left(seconds) do
    hours   = seconds |> div(60) |> div(60)
    minutes = div(seconds, 60) - hours * 60
    seconds = seconds - minutes * 60

    [hours, minutes, seconds]
    |> Enum.map(&String.pad_leading(to_string(&1), 2, "0"))
    |> Enum.join(":")
  end

  def time_left(%Enchantment{ability: %Ability{traits: %{"LongTerm" => lt_time}}} = enchantment) do
    lt_time * 60 - enchantment.time_elapsed_in_seconds
  end

  def next_tick_time(%Enchantment{} = enchantment) do
    min(67, time_left(enchantment))
  end

  def load_enchantments(%Item{instance_id: id} = item) do
    __MODULE__
    |> where([e], e.items_instances_id == ^id and e.finished == true)
    |> Repo.all
    |> Enum.reduce(item, fn enchantment, item ->
         ability = Ability.find(enchantment.ability_id)
         Ability.apply_item_enchantment(item, ability)
       end)
  end

end
