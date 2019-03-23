defmodule ApathyDrive.Commands.Return do
  use ApathyDrive.Command
  alias ApathyDrive.Monster

  def keywords, do: ["return"]

  def execute(%Room{} = room, %Monster{} = monster, _args) do
    room_exit = %{
      "kind" => "Action",
      "destination" => monster.spirit.class.start_room_id,
      "mover_message" =>
        "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
      "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
      "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
    }

    ApathyDrive.Commands.Move.execute(room, monster, room_exit, false)
  end
end
