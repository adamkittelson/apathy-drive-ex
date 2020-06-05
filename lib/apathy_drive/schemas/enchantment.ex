defmodule ApathyDrive.Enchantment do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityDamageType,
    AbilityTrait,
    Character,
    CraftingRecipe,
    DamageType,
    Enchantment,
    Item,
    ItemInstance,
    Match,
    Mobile,
    Room,
    Skill,
    SkillAttribute,
    TimerManager
  }

  schema "enchantments" do
    field(:finished, :boolean, default: false)
    field(:time_elapsed_in_seconds, :integer, default: 0)
    field(:value, ApathyDrive.JSONB)

    belongs_to(:items_instances, ItemInstance)
    belongs_to(:ability, Ability)
    belongs_to(:skill, Skill)
  end

  # crafting an item
  def tick(%Room{} = room, time, enchanter_ref, %Enchantment{ability_id: nil} = enchantment) do
    Room.update_mobile(room, enchanter_ref, fn _room, enchanter ->
      if !present?(enchanter, enchantment.items_instances_id) do
        message = "<p><span class='cyan'>You interrupt your work.</span></p>"

        Character.send_chat(enchanter, message)

        enchanter
      else
        {:ok, enchantment} =
          enchantment
          |> Ecto.Changeset.change(%{
            time_elapsed_in_seconds: enchantment.time_elapsed_in_seconds + time
          })
          |> Repo.update()

        enchantment =
          enchantment
          |> Repo.preload(:items_instances)
          |> update_in([Access.key!(:items_instances)], &Repo.preload(&1, :item))

        enchantment =
          enchantment
          |> put_in(
            [Access.key!(:items_instances), Access.key!(:item)],
            Item.from_assoc(enchantment.items_instances)
          )

        item = enchantment.items_instances.item

        time_left = time_left(enchanter, enchantment)

        if time_left <= 0 do
          Mobile.send_scroll(enchanter, "<p><span class='cyan'>You finish your work!</span></p>")

          enchantment
          |> Repo.delete!()

          message = "<p><span class='blue'>You've finished crafting #{item.name}.</span></p>"

          Character.send_chat(enchanter, message)

          enchanter
          |> add_enchantment_exp(enchantment)
          |> Character.load_items()
        else
          Mobile.send_scroll(
            enchanter,
            "<p><span class='dark-cyan'>You continue you work on the #{item.name}.</span></p>"
          )

          Mobile.send_scroll(
            enchanter,
            "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
              formatted_time_left(time_left)
            }</span></p>"
          )

          next_tick_time = next_tick_time(enchanter, enchantment)

          enchanter =
            enchanter
            |> TimerManager.send_after(
              {{:longterm, enchantment.items_instances_id}, :timer.seconds(next_tick_time),
               {:lt_tick, next_tick_time, enchanter_ref, enchantment}}
            )

          add_enchantment_exp(enchanter, enchantment)
        end
      end
    end)
  end

  def tick(%Room{} = room, time, enchanter_ref, %Enchantment{} = enchantment) do
    Room.update_mobile(room, enchanter_ref, fn _room, enchanter ->
      if !Enum.all?(enchantment.ability.traits["RequireItems"], &present?(enchanter, &1)) do
        message = "<p><span class='cyan'>You interrupt your work.</span></p>"

        Character.send_chat(enchanter, message)

        enchanter
      else
        roll = :rand.uniform()

        {:ok, enchantment} =
          if roll > 0.01 do
            enchantment
            |> Ecto.Changeset.change(%{
              time_elapsed_in_seconds: enchantment.time_elapsed_in_seconds + time
            })
            |> Repo.update()
          else
            message =
              "<p><span class='magenta'>You fumble the work! Cursing, you start over.</span></p>"

            Character.send_chat(enchanter, message)

            enchantment
            |> Ecto.Changeset.change(%{
              time_elapsed_in_seconds: 0
            })
            |> Repo.update()
          end

        item =
          enchantment
          |> Repo.preload(:items_instances)
          |> Map.get(:items_instances)
          |> Repo.preload(:item)
          |> Item.from_assoc()

        time_left = time_left(enchanter, enchantment)

        if time_left <= 0 do
          Mobile.send_scroll(enchanter, "<p><span class='cyan'>You finish your work!</span></p>")

          enchanter =
            if instance_id = enchantment.ability.traits["DestroyItem"] do
              scroll =
                (enchanter.inventory ++ enchanter.equipment)
                |> Enum.find(&(&1.instance_id == instance_id))

              Mobile.send_scroll(
                enchanter,
                "<p>As you read the #{scroll.name} it crumbles to dust.</p>"
              )

              ItemInstance
              |> Repo.get!(instance_id)
              |> Repo.delete!()

              enchanter
              |> Character.load_abilities()
              |> Character.load_items()
            else
              enchanter
            end

          enchantment =
            enchantment
            |> Ecto.Changeset.change(%{finished: true})
            |> Repo.update!()

          if Map.has_key?(enchantment.ability.traits, "Claimed") do
            enchantment
            |> Ecto.Changeset.change(%{value: enchanter.id})
            |> Repo.update!()
          end

          message =
            "<p><span class='blue'>You've enchanted #{item.name} with #{enchantment.ability.name}.</span></p>"

          Character.send_chat(enchanter, message)

          enchanter
          |> add_enchantment_exp(enchantment)
          |> Map.put(:enchantment, nil)
          |> Character.load_items()
        else
          Mobile.send_scroll(enchanter, "<p>#{enchantment.ability.traits["TickMessage"]}</p>")

          Mobile.send_scroll(
            enchanter,
            "<p><span class='dark-green'>Time Left:</span> <span class='dark-cyan'>#{
              formatted_time_left(time_left)
            }</span></p>"
          )

          next_tick_time = next_tick_time(enchanter, enchantment)

          enchanter =
            enchanter
            |> TimerManager.send_after(
              {{:longterm, enchantment.items_instances_id}, :timer.seconds(next_tick_time),
               {:lt_tick, next_tick_time, enchanter_ref, enchantment}}
            )

          enchanter
          |> add_enchantment_exp(enchantment)
          |> Map.put(:enchantment, enchantment)
        end
      end
    end)
  end

  def add_enchantment_exp(enchanter, %{ability_id: nil} = enchantment) do
    recipe = CraftingRecipe.for_item(enchantment.items_instances.item)

    skill =
      Skill
      |> Repo.get(recipe.skill_id)
      |> Map.put(:attributes, SkillAttribute.attributes(recipe.skill_id))

    exp = enchantment_exp(enchanter, skill.name)

    Enum.reduce(skill.attributes, enchanter, fn attribute, enchanter ->
      Character.add_attribute_experience(enchanter, %{
        attribute => 1 / length(skill.attributes)
      })
    end)
    |> ApathyDrive.Character.add_experience_to_buffer(exp)
  end

  def add_enchantment_exp(enchanter, enchantment) do
    exp = enchantment_exp(enchanter)

    skill = Repo.get(Skill, enchantment.ability.skill_id)

    enchanter = Character.add_skill_experience(enchanter, skill.name, exp)

    Enum.reduce(enchantment.ability.attributes, enchanter, fn attribute, enchanter ->
      attribute = String.to_atom(attribute)

      Character.add_attribute_experience(enchanter, %{
        attribute => 1 / length(enchantment.ability.attributes)
      })
    end)
    |> ApathyDrive.Character.add_experience_to_buffer(exp)
  end

  def present?(%Character{} = enchanter, instance_id) do
    item =
      (enchanter.inventory ++ enchanter.equipment)
      |> Enum.find(&(&1.instance_id == instance_id))

    !!item
  end

  def formatted_time_left(seconds) do
    hours = seconds |> div(60) |> div(60)
    minutes = div(seconds, 60) - hours * 60
    seconds = seconds - minutes * 60 - hours * 60 * 60

    [hours, minutes, seconds]
    |> Enum.map(&String.pad_leading(to_string(&1), 2, "0"))
    |> Enum.join(":")
  end

  def enchantment_exp(character, skill \\ nil) do
    rate =
      if skill do
        level = character.skills[skill].level

        character
        |> Map.put(:level, level)
        |> Character.drain_rate()
      else
        Character.drain_rate(character)
      end

    rate * 80
  end

  def total_enchantment_time(
        enchanter,
        %Enchantment{ability: %Ability{cast_time: cast_time}}
      ) do
    enchantment_level =
      case Map.get(enchanter.skills, "enchanting") do
        nil ->
          1

        skill ->
          skill.level
      end

    total_enchantment_time(enchantment_level, cast_time)
  end

  def total_enchantment_time(skill_level, cast_time) do
    trunc(cast_time * 25 / (25 + (2.5 * skill_level - 1)))
  end

  def time_left(enchanter, %Enchantment{} = enchantment) do
    total_enchantment_time(enchanter, enchantment) - enchantment.time_elapsed_in_seconds
  end

  def next_tick_time(enchanter, %Enchantment{} = enchantment) do
    min(67, time_left(enchanter, enchantment))
  end

  def count(%Item{instance_id: nil}), do: 0

  def count(%Item{} = item) do
    query =
      from e in Enchantment,
        where: e.items_instances_id == ^item.instance_id and e.finished == true,
        select: count()

    Repo.one(query)
  end

  def max_stacks?(%Item{} = item, %Ability{} = ability) do
    stack_count = ability.traits["StackCount"] || 1

    max_stacks? = count(item, ability) >= stack_count

    lock_count =
      Enchantment
      |> Ecto.Query.where([e], e.items_instances_id == ^item.instance_id)
      |> Ecto.Query.preload([:ability])
      |> Repo.all()
      |> Enum.filter(&(&1.ability.kind != "long-term"))
      |> length()

    max_stacks? or lock_count >= 2
  end

  def count(%Item{} = item, %Ability{} = ability) do
    stack_key = ability.traits["StackKey"] || ability.id

    query =
      from e in Enchantment,
        where: e.items_instances_id == ^item.instance_id and e.finished == true

    query
    |> Repo.all()
    |> Enum.map(&Ability.find(&1.ability_id))
    |> Enum.filter(&((&1.traits["StackKey"] || &1.id) == stack_key))
    |> Enum.count()
  end

  def copper_value(%Item{instance_id: nil}), do: 0

  def copper_value(%Item{} = item) do
    Enchantment
    |> Ecto.Query.where(
      [e],
      e.items_instances_id == ^item.instance_id
    )
    |> Ecto.Query.preload(:ability)
    |> Repo.all()
    |> Enum.map(&(&1.ability.cast_time || 0))
    |> Enum.sum()
  end

  def load_enchantments(%Item{instance_id: nil} = item),
    do: Map.put(item, :keywords, Match.keywords(item.name))

  def load_enchantments(%Item{instance_id: id} = item) do
    item =
      Enchantment
      |> Ecto.Query.where(
        [e],
        e.items_instances_id == ^item.instance_id and is_nil(e.ability_id)
      )
      |> Ecto.Query.preload(:skill)
      |> Repo.all()
      |> case do
        [%Enchantment{finished: false}] ->
          item
          |> Map.put(:unfinished, true)
          |> Map.put(:keywords, ["unfinished" | Match.keywords(item.name)])

        _ ->
          item =
            __MODULE__
            |> where([ia], ia.items_instances_id == ^id)
            |> preload([:ability])
            |> Repo.all()
            |> Enum.reduce(item, fn enchantment, item ->
              if enchantment.finished do
                ability = enchantment.ability

                traits =
                  enchantment.ability.id
                  |> AbilityTrait.load_traits()

                traits =
                  case AbilityDamageType.load_damage(enchantment.ability.id) do
                    [] ->
                      traits

                    damage ->
                      if traits["Elemental"] do
                        elemental_damage = Enum.find(damage, &(&1.damage_type == "Unaspected"))
                        damage = List.delete(damage, elemental_damage)

                        lore = ApathyDrive.ElementalLores.lores()[enchantment.value]

                        elemental_damage =
                          Enum.map(lore.damage_types, fn damage ->
                            damage_type_id = Repo.get_by(DamageType, name: damage.damage_type).id

                            damage
                            |> Map.put(:min, elemental_damage.min)
                            |> Map.put(:max, elemental_damage.min)
                            |> Map.put(:damage_type_id, damage_type_id)
                          end)

                        traits
                        |> Map.put("WeaponDamage", elemental_damage ++ damage)
                        |> Map.delete("Elemental")
                      else
                        Map.put(traits, "WeaponDamage", damage)
                      end
                  end

                traits =
                  case traits do
                    %{"Claimed" => _} ->
                      if character = Repo.get(Character, enchantment.value) do
                        Map.put(traits, "Claimed", character.id)
                      else
                        Map.delete(traits, "Claimed")
                      end

                    %{} ->
                      traits
                  end

                traits =
                  traits
                  |> Map.put("Enchantment", true)
                  |> Map.put("stack_count", traits["StackCount"] || 1)
                  |> Map.put("stack_key", traits["StackKey"] || ability.id)
                  |> Map.delete("StackCount")
                  |> Map.delete("StackKey")

                ability = put_in(ability.traits, traits)

                # cond do
                #   ability.kind in ["attack", "curse"] and item.type == "Weapon" ->
                #     Map.put(traits, "OnHit", ability)

                #   ability.kind == "blessing" ->
                #     Map.put(traits, "Passive", ability)

                #   :else ->
                #     Map.put(traits, "Grant", ability)
                # end

                enchantment_name =
                  if ability.kind == "long-term" do
                    ability.name
                  else
                    if traits["Elemental"] do
                      "#{ability.name} (locked)"
                    else
                      String.replace(ability.name, "elemental", enchantment.value) <> " (locked)"
                    end
                  end

                weight =
                  if traits["MassReduction"] do
                    div(Repo.get(Item, item.id).weight, 2)
                  else
                    item.weight
                  end

                item
                |> Systems.Effect.add(ability.traits)
                |> Map.put(:enchantments, [enchantment_name | item.enchantments])
                |> Map.put(:weight, weight)
              else
                item
              end
            end)

          item
          |> Map.put(:unfinished, false)
          |> Map.put(:keywords, Match.keywords(item.name))
      end

    item
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
