defmodule ApathyDrive.Scripts.ElementalBolt do
  alias ApathyDrive.{Ability, Room}

  @damage %{min: 60, max: 100}

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
        name: "elemental bolt",
        energy: 0,
        mana: 0,
        user_message: "You fire a #{lore.name} bolt at {{target}} for {{amount}} damage!",
        target_message: "{{User}} fires a #{lore.name} bolt at you for {{amount}} damage!",
        spectator_message:
          "{{User}} fires a #{lore.name} bolt at {{target}} for {{amount}} damage!",
        traits: %{
          "Damage" => damage
        }
      }

      crit_types = Enum.map(ability.traits["Damage"], & &1.damage_type_id)

      ability = Map.put(ability, :crit_tables, crit_types)

      Ability.execute(room, mobile_ref, ability, [target_ref])
    end)
  end
end
