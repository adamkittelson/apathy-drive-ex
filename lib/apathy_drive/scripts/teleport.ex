defmodule ApathyDrive.Scripts.Teleport do
  alias ApathyDrive.{Character, Mobile, Monster, Repo, Room, RoomMonster}

  def execute(%Room{} = room, mobile_ref, _target) do
    Room.update_mobile(room, mobile_ref, fn _room, character ->
      case Mobile.ability_value(character, "Scry") do
        {Monster, room_monster_id} ->
          if room_monster = Repo.get(RoomMonster, room_monster_id) do
            teleport(room, character, room_monster.room_id)
          else
            teleport_fail(character)
          end

        {Character, character_id} ->
          if char = Repo.get(Character, character_id) do
            teleport(room, character, char.room_id)
          else
            teleport_fail(character)
          end

        other ->
          IO.inspect(other)

          Mobile.send_scroll(
            character,
            "<p><span class='dark-yellow'>You must scry a target before you can teleport!</span></p>"
          )
      end
    end)
  end

  def teleport(room, character, room_id) do
    character = Systems.Effect.remove_all_stacks(character, :scry)

    room = put_in(room.mobiles[character.ref], character)

    room_exit = %{
      "kind" => "Action",
      "destination" => room_id,
      "mover_message" =>
        "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
      "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
      "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
    }

    ApathyDrive.Commands.Move.execute(room, character, room_exit, 0)
  end

  def teleport_fail(character) do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-yellow'>You fail to teleport to your target!</span></p>"
    )

    Systems.Effect.remove_all_stacks(character, :scry)
  end
end
