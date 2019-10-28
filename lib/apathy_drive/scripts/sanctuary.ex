defmodule ApathyDrive.Scripts.Sanctuary do
  alias ApathyDrive.{Character, ItemInstance, Monster, Repo, Room}

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    sanctuary = 2

    room =
      Room.update_mobile(room, mobile_ref, fn room, mobile ->
        %ItemInstance{
          item_id: sanctuary,
          room_id: room.id,
          character_id: nil,
          dropped_for_character_id: mobile.id,
          equipped: false,
          hidden: false,
          delete_at: Timex.shift(DateTime.utc_now(), minutes: 24)
        }
        |> Repo.insert!()

        Room.load_items(room)
      end)

    enforce_sanctuary(room)
  end

  def enforce_sanctuary(room) do
    if Enum.any?(room.items, &(&1.id == 2)) do
      Enum.reduce(room.mobiles, room, fn
        {_ref, %Monster{alignment: "evil"} = mobile}, room ->
          kick_out(room, mobile)

        {_ref, %Character{} = mobile}, room ->
          if Character.alignment(mobile) == "evil" do
            kick_out(room, mobile)
          else
            room
          end

        _, room ->
          room
      end)
    else
      room
    end
  end

  def kick_out(room, mobile) do
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
            "<span class='dark-green'>The sanctuary spell repels you #{exit_direction}!</span>",
          "from_message" =>
            "<span class='yellow'>#{mobile.name}</span> <span class='dark-green'>is repelled #{
              exit_direction
            } by the sanctuary spell!</span>",
          "to_message" =>
            "<span class='yellow'>#{mobile.name}</span> <span class='dark-green'>enters from #{
              enter_direction
            }!</span>"
        })

      mobile = Map.put(mobile, :energy, mobile.max_energy)

      ApathyDrive.Commands.Move.execute(room, mobile, room_exit, false)
    else
      room
    end
  end
end
