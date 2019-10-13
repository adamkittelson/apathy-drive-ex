defmodule ApathyDrive.Scripts.ElementalPresence do
  alias ApathyDrive.{Ability, Room}

  @damage %{min: 4, max: 12}
  @lore_resistance 5

  def execute(%Room{} = room, mobile_ref, target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, character ->
      lore = character.lore

      damage =
        lore.damage_types
        |> Enum.map(fn damage_type ->
          Map.merge(damage_type, %{
            min: div(@damage.min, length(lore.damage_types)),
            max: div(@damage.max, length(lore.damage_types))
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
        |> Map.put("Damage", damage)
        |> Map.put("StatusMessage", "Your are protected by a presence of #{lore.name}!")
        |> Map.put("RemoveMessage", "The #{lore.name} presence retreats.")
        |> Map.put("DamageShield", true)
        |> Map.put(
          "DamageShieldUserMessage",
          "Your #{lore.name} presence strikes {{target}} for {{amount}} damage!"
        )
        |> Map.put(
          "DamageShieldTargetMessage",
          "{{User}}'s' #{lore.name} presence strikes you for {{amount}} damage!"
        )
        |> Map.put(
          "DamageShieldSpectatorMessage",
          "{{User}}'s' #{lore.name} presence strikes {{target}} for {{amount}} damage!"
        )

      ability = %Ability{
        id: 8156,
        kind: "blessing",
        name: "elemental presence",
        energy: 0,
        mana: 0,
        duration: 600,
        user_message: "You summon a #{lore.name} presence to protect you!",
        spectator_message: "{{User}} summons a #{lore.name} presence to protect them!",
        traits: traits
      }

      Ability.execute(room, mobile_ref, ability, [target_ref])
    end)
  end
end
