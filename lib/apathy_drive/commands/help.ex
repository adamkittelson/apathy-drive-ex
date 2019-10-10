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
    Mobile,
    Race,
    RaceTrait,
    Repo,
    Room
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

      [topic] ->
        help(character, topic)

      list ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(list, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)
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

    Mobile.send_scroll(
      character,
      "\n\n<p><span class='dark-green'>Command:</span> <span class='dark-cyan'>#{ability.command}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Kind:</span> <span class='dark-cyan'>#{ability.kind}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Targets:</span> <span class='dark-cyan'>#{ability.targets}</span></p>"
    )

    if ability.mana && ability.mana > 0 do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Mana Cost:</span> <span class='dark-cyan'>#{ability.mana}</span></p>"
      )
    end

    if ability.duration && ability.duration > 0 do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Duration:</span> <span class='dark-cyan'>#{ability.duration} seconds</span></p>"
      )
    end

    if ability.energy && ability.energy > 0 do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Energy:</span> <span class='dark-cyan'>#{ability.energy}</span></p>"
      )
    end

    chance = Mobile.spellcasting_at_level(character, character.level) + ability.difficulty

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Success Chance:</span> <span class='dark-cyan'>#{chance}%</span></p>"
    )

    if ability.kind == "long-term" do
      skill_ability =
        ApathyDrive.SkillAbility
        |> Repo.get_by(ability_id: ability.id)
        |> Repo.preload(:skill)

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Skill: </span><span class='dark-cyan'>#{
          skill_ability.skill.name
        }</span></p>"
      )

      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Skill Level: </span><span class='dark-cyan'>#{
          skill_ability.level
        }</span></p>"
      )
    else
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Required Level: </span><span class='dark-cyan'>#{
          ability.level
        }</span></p>"
      )
    end

    classes =
      ClassAbility
      |> Ecto.Query.where(ability_id: ^ability.id)
      |> Repo.all()
      |> Enum.map(fn ca ->
        Repo.get(Class, ca.class_id).name
      end)

    if Enum.any?(classes) do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>Learnable By: </span><span class='dark-cyan'>#{
          ApathyDrive.Commands.Inventory.to_sentence(classes)
        }</span></p>"
      )
    end

    traits = AbilityTrait.load_traits(ability.id)

    traits =
      case AbilityDamageType.load_damage(ability.id) do
        [] ->
          traits

        damage ->
          Map.put(traits, "Damage", damage)
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
    [Ability.match_by_name(query, true), Race.match_by_name(query)]
    |> List.flatten()
    |> Enum.reject(&is_nil/1)
  end

  defp massage_trait({"RemoveSpells", ids}, _character) do
    spells =
      ids
      |> Enum.map(fn id ->
        Repo.get!(Ability, id)
      end)
      |> Enum.filter(&(&1 in Ability.learnable_abilities()))
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

  defp massage_trait({"AC%", amount}, character) do
    ac_from_percent = Ability.ac_for_mitigation_at_level(amount, character.level)
    {"AC", ac_from_percent}
  end

  defp massage_trait({"MR%", amount}, character) do
    ac_from_percent = Ability.ac_for_mitigation_at_level(amount, character.level)
    {"MR", ac_from_percent}
  end

  defp massage_trait({"AffectsLiving", _}, _character), do: {"Only affects living targets", nil}
  defp massage_trait({"StatusMessage", _}, _character), do: nil
  defp massage_trait({"RemoveMessage", _}, _character), do: nil
  defp massage_trait({name, value}, _character), do: {name, inspect(value)}
end
