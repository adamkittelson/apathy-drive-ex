defmodule Abilities.Incarnation do
  use Systems.Ability
  use Timex

  def useable_by?(entity) do
    Components.Spirit.value(entity) == true
  end

  def execute(entity, nil) do
    send_message(entity, "scroll", "<p>\nIncarnate as what?\n\n'use incarnation at (sex) (race)'\n\n('help races' for a list of possible races.).</p>")
  end

  def execute(entity, target) do
    if Entity.has_component?(entity, Components.Name) do
      [_ | race] = String.split(target)
      race = Enum.join(race, " ")
      sex = Enum.find(genders, fn(gender) ->
              (String.split(target, "#{gender} ") |> length) > 1
            end)
      if sex do
        ["", race] = String.split(target, "#{sex} ")
        incarnate(entity, sex, race)
      else
        send_message(entity, "scroll", "<p>You must choose #{Enum.join(genders, " or ")} for the sex of your mortal avatar.</p>")
      end
    else
      send_message(entity, "scroll", "<p>You must choose your name before you can incarnate. 'set name (name)'</p>")
    end
  end

  def help do
    """
Incarnation allows you to become mortal as the race of your choosing.
More powerful races require a larger investment of power, leaving you less to spend on training in your mortal life.

Syntax: 'use incarnation at (sex) (race)'

'help races' to get a list of available races
"""
  end

  defp incarnate(entity, sex, race_name) do
    case Systems.Match.all(Races.all, :name_contains, race_name) do
      [race] ->
        Entity.add_component(entity, Components.Race, race)
        Entity.add_component(entity, Components.Stats, Components.Stats.value(race))
        Entity.add_component(entity, Components.Gender, sex)
        Entity.add_component(entity, Components.EyeColor, select_random(eye_colors))
        Entity.add_component(entity, Components.HairColor, select_random(hair_colors))
        Entity.add_component(entity, Components.HairLength, select_random(hair_lengths))
        Entity.add_component(entity, Components.HP, Systems.HP.max(entity))
        Entity.add_component(entity, Components.Mana, Systems.Mana.max(entity))
        Entity.add_component(entity, Components.Limbs, entity |> Components.Race.value |> Components.Limbs.value)
        Entity.add_component(entity, Components.Hunting, [])
        Entity.add_component(entity, Components.Attacks, %{})
        Entity.add_component(entity, Components.Items, [])
        Entity.add_component(entity, Components.Effects, %{})
        Components.Skills.set_base_skills(entity, Components.Module.value(race).skills)
        Components.Attacks.reset_attacks(entity)
        Entity.add_component(entity, Components.Combat, %{"break_at" => Date.convert(Date.now, :secs)})
        Components.Spirit.value(entity, false)
        Entities.save!(entity)
        send_message(entity, "scroll", "<p>Your new body materializes around you.</p>")
      [] ->
        send_message(entity, "scroll", "<p><span class='red'>There is no race by that name.</span></p>")
      matches ->
        match_names = matches |> Enum.map &(Components.Name.value(&1))
        send_message(entity, "scroll", "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>")
        Enum.each match_names, fn(match_name) ->
          send_message entity, "scroll", "<p>-- #{match_name}</p>"
        end
    end
  end

  defp select_random(list) do
    :random.seed(:erlang.now)
    list |> Enum.shuffle |> List.first
  end

  defp hair_lengths do
    ["short", "shoulder-length", "long", "waist-length", "ankle-length", "none"]
  end

  defp hair_colors do
    ["silver", "red", "brown", "dark-brown", "blonde", "green", "blue", "black", "white"]
  end

  defp eye_colors do
    ["yellow", "pale-blue", "sea-blue", "dark-blue", "grey-blue", "slate-grey",
     "bright-green", "forest-green", "pale-green", "chesnut-brown", "dark-brown",
     "hazel", "violet", "lavender", "golden", "black", "crimson"]
  end

  defp genders do
    ["female", "male"]
  end

end
