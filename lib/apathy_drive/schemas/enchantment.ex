defmodule ApathyDrive.Enchantment do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityDamageType,
    AbilityTrait,
    Character,
    CraftingRecipe,
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

          enchantment
          |> Ecto.Changeset.change(%{finished: true})
          |> Repo.update!()

          message =
            "<p><span class='blue'>You've enchanted #{item.name} with #{enchantment.ability.name}.</span></p>"

          Character.send_chat(enchanter, message)

          enchanter
          |> add_enchantment_exp(enchantment)
          |> Character.load_items()
        else
          Mobile.send_scroll(enchanter, "<p>#{enchantment.ability.traits["TickMessage"]}</p>")

          item = load_enchantments(item)

          roll = :rand.uniform()

          # 1% chance per tick to shatter
          if roll > 0.01 do
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
            |> Character.load_items()
          else
            ItemInstance
            |> Repo.get!(item.instance_id)
            |> Repo.delete!()

            message =
              "<p><span class='magenta'>The #{item.name} shatters into a million pieces!</span></p>"

            Character.send_chat(enchanter, message)

            Room.send_scroll(
              room,
              "<p><span class='magenta'>#{enchanter.name} shatters a #{item.name} into a million pieces!</span></p>",
              [enchanter]
            )

            Character.load_items(enchanter)
          end
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
    |> ApathyDrive.Character.add_skill_experience(skill.name, exp)
  end

  def add_enchantment_exp(enchanter, enchantment) do
    exp = enchantment_exp(enchanter)

    IO.inspect(enchantment.ability.attributes)

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

  def enchantment_exp(character, _skill \\ nil) do
    Character.drain_rate(character) * 8
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

    count(item, ability) >= stack_count
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
    |> Enum.map(& &1.ability.cast_time)
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

                ability = put_in(ability.traits, traits)

                ability =
                  case AbilityDamageType.load_damage(enchantment.ability.id) do
                    [] ->
                      ability

                    damage ->
                      update_in(ability.traits, &Map.put(&1, "WeaponDamage", damage))
                  end

                # cond do
                #   ability.kind in ["attack", "curse"] and item.type == "Weapon" ->
                #     Map.put(traits, "OnHit", ability)

                #   ability.kind == "blessing" ->
                #     Map.put(traits, "Passive", ability)

                #   :else ->
                #     Map.put(traits, "Grant", ability)
                # end

                item
                |> Systems.Effect.add(ability.traits)
                |> Map.put(:enchantments, [ability.name | item.enchantments])
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
