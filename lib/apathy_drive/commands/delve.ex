defmodule ApathyDrive.Commands.Delve do
  use ApathyDrive.Command
  alias ApathyDrive.{Room, RoomUnity}

  def keywords, do: ["delve"]

  def execute(%Mobile{room_id: room_id}) do
    room_id
    |> RoomServer.find
    |> RoomServer.delve(self)
  end

  def execute(mobile, _args) when is_pid(mobile) do
    Mobile.delve(mobile)
  end

  def execute(%Room{room_unity: %RoomUnity{essence_targets: targets}} = room, mobile) do
    Mobile.send_scroll(mobile, "\n\n<p><span class='dark-green'>Room:</span> <span class='dark-cyan'>#{room.id}</span>  <span class='dark-green'>Area:</span> <span class='dark-cyan'>#{room.area.name} (Level #{room.area.level})</span>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>x:</span> <span class='dark-cyan'>#{room.coordinates["x"]}</span> <span class='dark-green'>y:</span> <span class='dark-cyan'>#{room.coordinates["y"]}</span> <span class='dark-green'>z:</span> <span class='dark-cyan'>#{room.coordinates["z"]}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-magenta'>==================================================</span></p>")
    Enum.each(targets, fn({unity, %{"adjacent" => adj, "mobile" => mob, "local" => loc, "target" => target}}) ->
      case unity do
        "good" ->
          Mobile.send_scroll(mobile, "<p><span class='white'>Good Influence:</span></p>")
        "default" ->
          Mobile.send_scroll(mobile, "<p><span class='cyan'>Natural Influence:</span></p>")
        "evil" ->
          Mobile.send_scroll(mobile, "<p><span class='magenta'>Evil Influence:</span></p>")
      end

      Mobile.send_scroll(mobile, "<p><span class='dark-green'>From Adjacent Rooms:</span> <span class='dark-cyan'>#{average(adj)}</span><p>")
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>From Mobiles:</span> <span class='dark-cyan'>#{average(mob)}</span><p>")
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>From Room Essence:</span> <span class='dark-cyan'>#{average(loc)}</span><p>")
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>Target Essence:</span> <span class='dark-cyan'>#{max(0, trunc(target))}</span></p>\n")
    end)
  end

  defp average([]), do: 0
  defp average(list) do
    trunc(Enum.sum(list) / length(list))
  end

end
