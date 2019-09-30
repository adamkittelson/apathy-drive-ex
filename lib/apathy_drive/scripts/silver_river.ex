defmodule ApathyDrive.Scripts.SilverRiver do
  alias ApathyDrive.{Ability, Mobile, Room}

  @boats [
    # log raft
    690,
    # wooden skiff
    691,
    # silverbark canoe
    1181
  ]

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn character ->
      if Enum.any?(character.inventory, &(&1.id in @boats)) do
        character
      else
        Mobile.send_scroll(character, "<p>The river bashes you up against some rocks!</p>")
        Room.send_scroll(room, "<p>The river bashes you up against some rocks!</p>", [character])

        ability =
          754
          |> Ability.find()
          |> Map.put(:energy, 0)
          |> Map.put(:difficulty, nil)
          |> Map.put(:ignores_round_cooldown?, true)

        Ability.execute(room, mobile_ref, ability, [mobile_ref])
      end
    end)
  end
end
