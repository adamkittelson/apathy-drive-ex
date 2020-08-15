defmodule ApathyDrive.Commands.Help do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Ability,
    AbilityDamageType,
    AbilityTrait,
    Class,
    ClassAbility,
    Commands.Inventory,
    ElementalLores,
    Enchantment,
    Match,
    Mobile,
    Race,
    RaceTrait,
    Repo,
    Room,
    Skill,
    SkillAbility
  }

  require Ecto.Query

  def keywords, do: ["help"]

  def execute(%Room{} = room, character, []) do
    Mobile.send_scroll(
      character,
      "<p>Type <span class='yellow'>HELP</span> followed by a topic for help on that topic\n\n</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Tips</span>     - A few tips to help you get started.</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Commands</span> - A list of commands available within the game</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Stats1</span>   - An explanation of the statistics of your character</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Stats2</span>   - A continuation of stats1, including help on allocating your stats</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Combat</span>   - Everything you need to know about killing others</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Races</span>    - A list of the various races</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Classes</span>  - For help on a certain class, type Help <Classname></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Commun</span>   - Communicating with others in the realm</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Info</span>     - A list of information commands, and how to use them</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Spells</span>   - Everything you need to know about spellcasting</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Shops</span>    - Buying and selling of items in the Realm</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Laws</span>     - Before thinking of doing anything nasty, read this</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Movement</span> - How to travel throughout the Realm</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Party</span>    - You have friends? Well here's how to use them</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Items</span>    - Commands related to items within the game</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Help</span>     - A quick description of how to use the help system</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Topics</span>   - A list of all available topics</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Profile</span>  - Setting up your personal options within the game</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Misc</span>     - Miscellaneous commands</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>Set</span>      - Various toggleable profile options</p>"
    )

    room
  end

  def execute(%Room{} = room, character, ["lores"]) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>The following lores are available for use by Elementalists:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Level   Lore          Damage Types</span></p>"
    )

    ElementalLores.lores()
    |> Map.values()
    |> Enum.sort_by(& &1.level)
    |> Enum.each(fn lore ->
      level = to_string(lore.level) |> String.pad_leading(5)
      name = to_string(lore.name) |> String.pad_trailing(13)

      damage =
        lore.damage_types
        |> Enum.map(& &1.damage_type)
        |> Enum.sort()
        |> ApathyDrive.Commands.Inventory.to_sentence()

      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan'>#{level}   #{name} #{damage}</span></p>"
      )
    end)

    Mobile.send_scroll(
      character,
      "<p>\n<span class='dark-green'>Example usage:</span> <span class='dark-cyan'>\"use fire lore\"</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p>Once a lore is selected all elemental spells cast will be cast with the selected elemental college until a different lore is selected.</p>"
    )

    room
  end

  def execute(%Room{} = room, character, [arg]) when arg in ["races", "Races"] do
    Mobile.send_scroll(
      character,
      "<p>Type <span class='yellow'>HELP</span> <span class='cyan'>&lt;Race Name&gt;</span> for specific help on a race.\n\n</p>"
    )

    Race
    |> Repo.all()
    |> Enum.each(fn race ->
      Mobile.send_scroll(
        character,
        "<p>#{race.name}</p>"
      )
    end)

    room
  end

  def execute(%Room{} = room, character, [arg]) when arg in ["classes", "Classes"] do
    Mobile.send_scroll(
      character,
      "<p>Type <span class='yellow'>HELP</span> <span class='cyan'>&lt;Class Name&gt;</span> for specific help on a race.\n\n</p>"
    )

    Class
    |> Repo.all()
    |> Enum.sort_by(& &1.id)
    |> Enum.each(fn class ->
      Mobile.send_scroll(
        character,
        "<p>#{class.name}</p>"
      )
    end)

    room
  end

  def execute(%Room{} = room, character, [arg]) when arg in ["tips", "Tips"] do
    Mobile.send_scroll(
      character,
      "<p>Here are some helpful \"hints\" or advice to get you started...\n\n</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>1.</span> Read all of the help files, or at least glance through them. This will save you a lot of time and effort overall.\n\n</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>2.</span> Pick a class that you feel comfortable with. We have tried to make ALL of the classes equal in efficiency, but each one has its own unique features.\n\n</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>3.</span> Once you have your character made, check out all of the stores in Newhaven and equip yourself with weaponry, armour, and spells (if you are a spellcaster). Don't go down into the arena until you are armed and ready.\n\n</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>4.</span> To familiarize yourself with the combat system, go to the Newhaven Arena. Here you will be able to earn experience for your next level, and gold to purchase better weaponry and armour. Once you have enough experience to obtain your next level, go to the guild and train.\n\n</p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>5.</span> Now that you have completed these steps, you could try adventuring in the dungeon past the north door in the Newhaven Arena. Type \"bash north\" or \"picklock north\" (If you have the skill) to open the lock/door.\n\n</p>"
    )

    room
  end

  def execute(%Room{} = room, character, args) do
    query = Enum.join(args, " ")

    case topic(query) do
      [] ->
        Mobile.send_scroll(character, "<p>Sorry! No help is available for that topic.</p>")

      nil ->
        Mobile.send_scroll(character, "<p>Sorry! No help is available for that topic.</p>")

      list when is_list(list) ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(list, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

      topic ->
        help(character, topic)
    end

    room
  end

  def help(character, %Race{} = race) do
    traits = RaceTrait.load_traits(race.id)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>"
    )

    Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{race.name}</span></p>")

    Mobile.send_scroll(
      character,
      "<p>    #{race.description}</p>"
    )

    Mobile.send_scroll(
      character,
      "\n\n<p><span class='dark-green'>Strength:</span>  <span class='dark-cyan'>#{race.strength}</span>  <span class='dark-green'>Agility:</span> <span class='dark-cyan'>#{
        race.agility
      }</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Intellect:</span> <span class='dark-cyan'>#{race.intellect}</span>  <span class='dark-green'>Health:</span>  <span class='dark-cyan'>#{
        race.health
      }</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Willpower:</span> <span class='dark-cyan'>#{race.willpower}</span>  <span class='dark-green'>Charm:</span>   <span class='dark-cyan'>#{
        race.charm
      }</span></p>"
    )

    Mobile.send_scroll(character, "\n\n<p><span class='dark-green'>Traits:</span></p>")

    traits
    |> Enum.map(&massage_trait(&1, character))
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
    |> Enum.each(fn
      {name, nil} ->
        Mobile.send_scroll(character, "<p>  <span class='dark-green'>#{name}</span></p>")

      {name, value} ->
        Mobile.send_scroll(
          character,
          "<p>  <span class='dark-green'>#{name}:</span> <span class='dark-cyan'>#{value}</span></p>"
        )
    end)

    Mobile.send_scroll(
      character,
      "<p>  <span class='dark-green'>Stealth:</span> <span class='dark-cyan'>#{
        if race.stealth, do: "Yes", else: "No"
      }</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>"
    )
  end

  def help(character, %Class{id: class_id} = class) do
    traits =
      ApathyDrive.ClassAbility
      |> Ecto.Query.where(
        [ss],
        ss.class_id == ^class_id and is_nil(ss.level) and ss.auto_learn == true
      )
      |> Ecto.Query.select([:ability_id])
      |> Repo.one()
      |> Map.get(:ability_id)
      |> Ability.find()
      |> Map.get(:traits)

    abilities =
      ApathyDrive.ClassAbility
      |> Ecto.Query.where(
        [ss],
        ss.class_id == ^class_id and not is_nil(ss.level)
      )
      |> Repo.all()
      |> Enum.group_by(& &1.level)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>+-------------------------------------------------------------------+</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p> <span class='white'>#{String.pad_trailing(class.name, 48)}</span><span class='dark-green'>Exp Modifier:</span>  <span class='dark-cyan'>#{
        class.exp_modifier
      }%</p>"
    )

    Mobile.send_scroll(
      character,
      "<p style='max-width: 69ch; padding: 0 1ch;'>    #{class.description}</p>"
    )

    Mobile.send_scroll(character, "\n\n<p><span class='white'>Traits:</span></p>")

    traits
    |> Enum.map(&massage_trait(&1, character))
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
    |> Enum.each(fn
      {name, nil} ->
        Mobile.send_scroll(character, "<p>  <span class='dark-green'>#{name}</span></p>")

      {name, value} ->
        Mobile.send_scroll(
          character,
          "<p>  <span class='dark-green'>#{name}:</span> <span class='dark-cyan'>#{value}</span></p>"
        )
    end)

    if Enum.any?(abilities) do
      Mobile.send_scroll(character, "\n\n<p><span class='white'>Abilities:</span></p>")

      Mobile.send_scroll(character, "<p>  <span class='dark-magenta'>Level    Name</span></p>")

      abilities
      |> Enum.sort_by(fn {level, _} -> level end)
      |> Enum.each(fn {level, abilities} ->
        abilities =
          abilities
          |> Enum.map(&ability_name(&1))
          |> ApathyDrive.Commands.Inventory.to_sentence()

        level =
          level
          |> to_string()
          |> String.pad_trailing(9)

        Mobile.send_scroll(
          character,
          "<p>  <span class='dark-cyan'>#{level}#{abilities}</span></p>"
        )
      end)
    end

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>"
    )
  end

  def help(character, %Ability{} = ability) do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>"
    )

    Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{ability.name}</span></p>")

    Mobile.send_scroll(
      character,
      "<p>    #{ability.description}</p>"
    )

    if ability.kind != "passive" do
      Mobile.send_scroll(
        character,
        "\n\n<p><span class='dark-green'>Command:</span> <span class='dark-cyan'>#{
          ability.command
        }</span></p>"
      )
    end

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Kind:</span> <span class='dark-cyan'>#{ability.kind}</span></p>"
    )

    if ability.kind != "passive" do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Targets:</span> <span class='dark-cyan'>#{ability.targets}</span></p>"
      )
    end

    if ability.mana && ability.mana > 0 && ability.kind != "passive" do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Mana Cost:</span> <span class='dark-cyan'>#{ability.mana}</span></p>"
      )
    end

    ability =
      Map.put(ability, :attributes, ApathyDrive.AbilityAttribute.load_attributes(ability.id))

    chance =
      if ability.difficulty do
        Mobile.send_scroll(
          character,
          "<p><span class='dark-green'>Attributes:</span> <span class='dark-cyan'>#{
            Enum.join(ability.attributes, ", ")
          }</span></p>"
        )

        Mobile.spellcasting_at_level(character, character.level, ability) + ability.difficulty
      else
        100
      end

    if ability.duration && ability.kind != "passive" do
      cond do
        ability.duration > 0 ->
          Mobile.send_scroll(
            character,
            "<p><span class='dark-green'>Duration:</span> <span class='dark-cyan'>#{
              ability.duration
            } seconds</span></p>"
          )

        ability.duration == -1 ->
          Mobile.send_scroll(
            character,
            "<p><span class='dark-green'>Duration:</span> <span class='dark-cyan'>until logout</span></p>"
          )

        :else ->
          :noop
      end
    end

    if ability.energy && ability.energy > 0 && ability.kind != "passive" do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Energy:</span> <span class='dark-cyan'>#{ability.energy}</span></p>"
      )
    end

    if ability.kind != "passive" do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Success Chance:</span> <span class='dark-cyan'>#{
          min(100, chance)
        }%</span></p>"
      )
    end

    if ability.cast_time do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Enchant Time: </span><span class='dark-cyan'>#{
          Float.round(
            Enchantment.total_enchantment_time(character, %Enchantment{ability: ability}) / 60,
            2
          )
        } minutes</span></p>"
      )
    end

    classes =
      ClassAbility
      |> Ecto.Query.where(ability_id: ^ability.id)
      |> Repo.all()
      |> Enum.map(fn %{class_id: id, auto_learn: auto_learn, level: level} ->
        name = Repo.get(Class, id).name

        color =
          if auto_learn do
            "dark-magenta"
          else
            "dark-cyan"
          end

        "<span class='#{color}'>#{name} (#{level})</span>"
      end)

    if Enum.any?(classes) do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Classes: </span><span class='dark-cyan'>#{
          ApathyDrive.Commands.Inventory.to_sentence(classes)
        }</span></p>"
      )
    end

    skills =
      SkillAbility
      |> Ecto.Query.where(ability_id: ^ability.id)
      |> Repo.all()
      |> Enum.map(fn ca ->
        Repo.get(Skill, ca.skill_id).name
      end)

    if Enum.any?(skills) do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Skills: </span><span class='dark-cyan'>#{
          ApathyDrive.Commands.Inventory.to_sentence(skills)
        }</span></p>"
      )
    end

    traits = AbilityTrait.load_traits(ability.id)

    traits =
      case AbilityDamageType.load_damage(ability.id) do
        [] ->
          traits

        damage ->
          traits =
            if ability.mana && ability.mana > 0 do
              count = length(damage)
              bonus_damage = Character.base_spell_damage(character, ability) * 0.1 / count

              damage =
                Enum.map(damage, fn element ->
                  element
                  |> Map.update(:min, 0, &trunc(&1 + bonus_damage))
                  |> Map.update(:max, 0, &trunc(&1 + bonus_damage))
                end)

              Map.put(traits, "Damage", damage)
            else
              Map.put(traits, "Damage", damage)
            end

          damage = traits["Damage"]

          if traits["Elemental"] do
            elemental_damage = Enum.find(damage, &(&1.damage_type == "Unaspected"))

            if elemental_damage do
              damage = List.delete(damage, elemental_damage)

              if lore = character.lore do
                elemental_damage =
                  Enum.map(lore.damage_types, fn damage ->
                    damage
                    |> Map.put(:min, elemental_damage.min)
                    |> Map.put(:max, elemental_damage.min)
                  end)

                Map.put(traits, "Damage", elemental_damage ++ damage)
              else
                elemental_damage = Map.put(elemental_damage, :damage_type, "Elemental")
                Map.put(traits, "Damage", [elemental_damage | damage])
              end
            else
              if lore = character.lore do
                min =
                  damage
                  |> Enum.map(& &1.min)
                  |> Enum.sum()

                max =
                  damage
                  |> Enum.map(& &1.max)
                  |> Enum.sum()

                damage =
                  Enum.reduce(damage, [], fn type, damage ->
                    type =
                      type
                      |> Map.put(:max, max(1, div(max, 2)))
                      |> Map.put(:min, max(1, div(min, 2)))

                    [type | damage]
                  end)

                lore_min =
                  min
                  |> div(2)
                  |> div(length(lore.damage_types))
                  |> max(1)

                lore_max =
                  max
                  |> div(2)
                  |> div(length(lore.damage_types))
                  |> max(1)

                damage =
                  Enum.reduce(lore.damage_types, damage, fn type, damage ->
                    type =
                      type
                      |> Map.put(:min, lore_min)
                      |> Map.put(:max, lore_max)

                    [type | damage]
                  end)

                Map.put(traits, "Damage", damage)
              else
                Map.put(traits, "Damage", damage)
              end
            end
          else
            Map.put(traits, "Damage", damage)
          end
      end

    if ability.kind != "passive" do
      if ability.user_message do
        Mobile.send_scroll(
          character,
          "<p>\n<span class='dark-green'>User Message: </span><span class='dark-cyan'>#{
            ability.user_message
          }</span></p>"
        )
      end

      if ability.target_message do
        Mobile.send_scroll(
          character,
          "<p><span class='dark-green'>Target Message: </span><span class='dark-cyan'>#{
            ability.target_message
          }</span></p>"
        )
      end

      if ability.spectator_message do
        Mobile.send_scroll(
          character,
          "<p><span class='dark-green'>Spectator Message: </span><span class='dark-cyan'>#{
            ability.spectator_message
          }</span></p>"
        )
      end

      if traits["RemoveMessage"] do
        Mobile.send_scroll(
          character,
          "<p><span class='dark-green'>Remove Message: </span><span class='dark-cyan'>#{
            traits["RemoveMessage"]
          }</span></p>"
        )
      end

      if traits["StatusMessage"] do
        Mobile.send_scroll(
          character,
          "<p><span class='dark-green'>Status Message: </span><span class='dark-cyan'>#{
            traits["StatusMessage"]
          }</span></p>"
        )
      end
    end

    Mobile.send_scroll(character, "\n\n<p><span class='dark-green'>Effects:</span></p>")

    traits
    |> Enum.map(&massage_trait(&1, character))
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
    |> Enum.each(fn
      {name, nil} ->
        Mobile.send_scroll(character, "<p>  <span class='dark-green'>#{name}</span></p>")

      {name, value} ->
        Mobile.send_scroll(
          character,
          "<p>  <span class='dark-green'>#{name}:</span> <span class='dark-cyan'>#{value}</span></p>"
        )
    end)

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>+------------------------------------------------------------------+</span></p>"
    )
  end

  def topic(query) do
    [Race.match_by_name(query), Class.match_by_name(query), Ability.match_by_name(query, true)]
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Match.all(:keyword_starts_with, query)
  end

  defp massage_trait({"RemoveSpells", ids}, _character) do
    spells =
      ids
      |> Enum.map(fn id ->
        Repo.get!(Ability, id)
      end)
      |> Enum.map(& &1.name)

    if Enum.any?(spells) do
      {"Removes the following spells", Inventory.to_sentence(spells)}
    end
  end

  defp massage_trait({"Heal", %{"max" => max, "min" => min}}, _character) do
    {"Restores", "#{min}-#{max} HP"}
  end

  defp massage_trait({"Damage", damages}, _character) do
    Enum.map(damages, fn %{damage_type: type, kind: kind, max: max, min: min} ->
      {"Damage", "#{min}-#{max} #{kind} damage (#{String.downcase(type)})"}
    end)
  end

  defp massage_trait({"Dodge", amount}, _character) do
    {"Modifies Dodge Skill By", amount}
  end

  defp massage_trait({"EndCast", id}, _character) do
    name = Repo.get(Ability, id).name
    {"EndCast", name}
  end

  defp massage_trait({"ClassCombatLevel", value}, _character) do
    {"Combat Proficiency", Character.combat_proficiency(value)}
  end

  defp massage_trait({"MaxHP", amount}, _character) do
    {"Bonus HP per level", amount}
  end

  defp massage_trait({"MaxMana", amount}, _character) do
    {"Bonus Mana per level", amount}
  end

  defp massage_trait({"AC%", amount}, _character) do
    ac_from_percent = Ability.ac_for_mitigation_at_level(amount)
    {"AC", ac_from_percent}
  end

  defp massage_trait({"Powerstone", _value}, _character) do
    {"<span class='dark-cyan'>Transforms a stone or gem into a mana granting powerstone</span>",
     nil}
  end

  defp massage_trait({"MR%", amount}, _character) do
    ac_from_percent = Ability.ac_for_mitigation_at_level(amount)
    {"MR", ac_from_percent}
  end

  defp massage_trait({"AffectsLiving", _}, _character), do: {"Only affects living targets", nil}
  defp massage_trait({"StatusMessage", _}, _character), do: nil
  defp massage_trait({"RemoveMessage", _}, _character), do: nil
  defp massage_trait({name, value}, _character), do: {name, inspect(value)}

  defp ability_name(%{ability_id: id, auto_learn: auto_learn}) do
    name = Ability.find(id).name

    color =
      if auto_learn do
        "dark-magenta"
      else
        "dark-cyan"
      end

    "<span class='#{color}'>#{name}</span>"
  end
end
