defmodule ApathyDrive.Scripts.ElementalTransformation do
  alias ApathyDrive.{Ability, Room}

  @damage %{min: 5, max: 10}
  @lore_resistance 50

  def execute(%Room{} = room, mobile_ref, target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, character ->
      lore = character.lore

      damage =
        lore.damage_types
        |> Enum.map(fn damage_type ->
          Map.merge(damage_type, %{
            min: @damage.min,
            max: @damage.max
          })
        end)

      traits =
        ApathyDrive.Commands.Protection.damage_types()
        |> Map.keys()
        |> Enum.reduce(%{}, fn damage_type, traits ->
          if damage_type in Enum.map(lore.damage_types, & &1.damage_type) do
            Map.put(traits, "Resist" <> damage_type, @lore_resistance)
          else
            traits
          end
        end)
        |> Map.put("WeaponDamage", damage)
        |> Map.put("StatusMessage", "Your have a body of #{lore.name}!")
        |> Map.put("RemoveMessage", "Your body returns to normal.")
        |> Map.put("StackKey", "elemental transformation")
        |> Map.put("Strength", 10)
        |> Map.put("Agility", 10)
        |> Map.put("Intellect", 10)
        |> Map.put("Willpower", 10)
        |> Map.put("Charm", 10)
        |> Map.put("Health", 10)

      ability = %Ability{
        id: 8110,
        kind: "blessing",
        name: "elemental transformation",
        energy: 0,
        mana: 0,
        duration: 600,
        user_message: "You transform your body into a form of #{lore.name}!",
        spectator_message: "{{User}} transforms into a form of #{lore.name}!",
        traits: traits
      }

      Ability.execute(room, mobile_ref, ability, [target_ref])
    end)
  end
end
