defmodule ApathyDrive.Commands.System.Misc do
  alias ApathyDrive.{Area, Mobile, Room}
  require Ecto.Query

  def execute(%Room{} = room, character, ["goto" | area]) do
    area = Enum.join(area, " ")

    area
    |> Area.match_by_name
    |> case do
         %Area{} = area ->
           Ecto.assoc(area, :rooms)
           |> Ecto.Query.where([r], not is_nil(r.coordinates))
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

                  ApathyDrive.Commands.Move.execute(room, character, room_exit)
                _ ->
                  Mobile.send_scroll(character, "<p>#{area.name} has no rooms!</p>")
                  room
              end
         nil ->
           Mobile.send_scroll(character, "<p>Could not find an area named \"#{area}\".")
           room
       end
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end
end