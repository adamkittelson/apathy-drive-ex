defmodule ApathyDrive.Scripts.LesserElementalStorm do
  alias ApathyDrive.{Ability, Room}

  @damage %{min: 16, max: 50}

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

      ability = %Ability{
        kind: "attack",
        name: "lesser elemental storm",
        energy: 0,
        mana: 0,
        user_message: "A lesser bolt of #{lore.name} strikes {{target}} for {{amount}} damage!",
        target_message: "A lesser bolt of #{lore.name} strikes you for {{amount}} damage!",
        spectator_message:
          "A lesser bolt of #{lore.name} strikes {{target}} for {{amount}} damage!",
        traits: %{
          "Damage" => damage
        }
      }

      Ability.execute(room, mobile_ref, ability, [target_ref])
    end)
  end
end
