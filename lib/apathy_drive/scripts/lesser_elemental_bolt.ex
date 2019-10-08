defmodule ApathyDrive.Scripts.LesserElementalBolt do
  alias ApathyDrive.{Ability, Mobile, Room}

  @damage %{min: 8, max: 25}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, character ->
      if lore = character.lore do
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
          name: "lesser elemental bolt",
          energy: 1000,
          mana: 3,
          spell?: true,
          attributes: %{intellect: 1, willpower: 1},
          user_message:
            "You fire a lesser #{lore.name} bolt at {{target}} for {{amount}} damage!",
          target_message:
            "{{User}} fires a lesser #{lore.name} bolt at you for {{amount}} damage!",
          spectator_message:
            "{{User}} fires a lesser #{lore.name} bolt at {{target}} for {{amount}} damage!",
          traits: %{
            "Damage" => damage
          }
        }

        crit_types = Enum.map(ability.traits["Damage"], & &1.damage_type_id)

        ability = Map.put(ability, :crit_tables, crit_types)

        Ability.execute(room, mobile_ref, ability, [target_ref])
      else
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Elemental spells require an active lore! (use &lt;lore&gt; lore)</span></p>"
        )
      end
    end)
  end
end
