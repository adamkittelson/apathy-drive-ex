defmodule ApathyDrive.Commands.Teleport do
  use ApathyDrive.Command
  alias ApathyDrive.{Area, Class, Mobile}
  require Ecto.Query

  def keywords, do: ["teleport", "goto"]

  def execute(%Room{} = room, %Mobile{monster_template_id: nil, spirit: %Spirit{class: %Class{unities: unities}}} = mobile, args) do
    area = Enum.join(args, " ")

    area
    |> Area.match_by_name
    |> case do
         %Area{} = area ->
           Ecto.assoc(area, :rooms)
           |> Ecto.Query.join(:inner, [r], ru in ApathyDrive.RoomUnity, ru.room_id == r.id)
           |> Ecto.Query.where([r, ru], not is_nil(r.coordinates) and ru.controlled_by in ^unities)
           |> Ecto.Query.limit(1)
           |> Ecto.Query.select([:id])
           |> ApathyDrive.Repo.one
           |> case do
                %Room{id: room_id} ->
                  room_exit =
                    %{
                      "kind" => "Action",
                      "destination" => room_id,
                      "mover_message" => "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
                      "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
                      "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
                    }

                  ApathyDrive.Commands.Move.execute(room, mobile, room_exit)
                _ ->
                  Mobile.send_scroll(mobile, "<p>#{area.name} has no rooms you can teleport to!</p>")
                  room
              end
         nil ->
           Mobile.send_scroll(mobile, "<p>Could not find an area named \"#{area}\".")
           room
       end
  end
  def execute(%Room{} = room, %Mobile{} = mobile, _args) do
    Mobile.send_scroll(mobile, "<p>You need to <span class='green'>unpossess</span> #{mobile.name} in order to teleport</p>")
    room
  end

end
