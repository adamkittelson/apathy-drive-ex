defmodule ApathyDrive.Scripts.KnockBack do
  alias ApathyDrive.Room

  def execute(%Room{} = room, _mobile_ref, target_ref) do
    Room.update_mobile(room, target_ref, fn room, mobile ->
      exits =
        case room.exits do
          nil ->
            []

          _exits ->
            ApathyDrive.AI.exits_in_area(room, mobile)
        end

      if Enum.any?(exits) do
        room_exit = Enum.random(exits)

        mirror_exit = Room.mirror_exit(room, room_exit["destination"])

        exit_direction = Room.exit_direction(room_exit["direction"])
        enter_direction = Room.enter_direction(mirror_exit["direction"])

        room_exit =
          room_exit
          |> Map.merge(%{
            "kind" => "Action",
            "mover_message" =>
              "<span class='dark-green'>You fly #{exit_direction} very fast!</span>",
            "from_message" =>
              "<span class='yellow'>#{mobile.name}</span> <span class='dark-green'>flies #{
                exit_direction
              } very fast!</span>",
            "to_message" =>
              "<span class='yellow'>#{mobile.name}</span> <span class='dark-green'>flies in from #{
                enter_direction
              } very fast!</span>"
          })

        mobile = Map.put(mobile, :energy, mobile.max_energy)

        ApathyDrive.Commands.Move.execute(room, mobile, room_exit, 0)
      else
        room
      end
    end)
  end
end
