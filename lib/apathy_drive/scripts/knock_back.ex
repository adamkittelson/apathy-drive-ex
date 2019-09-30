defmodule ApathyDrive.Scripts.KnockBack do
  alias ApathyDrive.Room

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
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

        ApathyDrive.Commands.Move.execute(room, mobile, room_exit, false)
      else
        room
      end
    end)
  end
end
