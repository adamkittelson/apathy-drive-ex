defmodule ApathyDrive.Commands.Delve do
  use ApathyDrive.Command
  alias ApathyDrive.{Room, RoomUnity}

  def keywords, do: ["delve"]

  def execute(%Room{room_unity: %RoomUnity{essence_targets: targets}} = room, %Mobile{} = mobile, _args) do
    Mobile.send_scroll(mobile, "\n\n<p><span class='dark-green'>Room:</span> <span class='dark-cyan'>#{room.id}</span>  <span class='dark-green'>Area:</span> <span class='dark-cyan'>#{room.area.name} (Level #{room.area.level})</span>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>x:</span> <span class='dark-cyan'>#{room.coordinates["x"]}</span> <span class='dark-green'>y:</span> <span class='dark-cyan'>#{room.coordinates["y"]}</span> <span class='dark-green'>z:</span> <span class='dark-cyan'>#{room.coordinates["z"]}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-magenta'>==================================================</span></p>")
    Enum.each(targets, fn({unity, %{"adjacent" => adj, "mobile" => mob, "target" => target} = targets}) ->
      case unity do
        "good" ->
          Mobile.send_scroll(mobile, "<p><span class='white'>Good Influence:</span></p>")
        "default" ->
          Mobile.send_scroll(mobile, "<p><span class='cyan'>Natural Influence:</span></p>")
        "evil" ->
          Mobile.send_scroll(mobile, "<p><span class='magenta'>Evil Influence:</span></p>")
      end

      if targets["control"], do: Mobile.send_scroll(mobile, "<p><span class='dark-green'>Nexus:</span> <span class='dark-cyan'>#{trunc(targets["control"])}</span><p>")
      if adj, do: Mobile.send_scroll(mobile, "<p><span class='dark-green'>Adjacent:</span> <span class='dark-cyan'>#{trunc(adj)}</span><p>")
      if mob, do: Mobile.send_scroll(mobile, "<p><span class='dark-green'>Mobiles:</span> <span class='dark-cyan'>#{trunc(mob)}</span><p>")
      
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>Target Essence:</span> <span class='dark-cyan'>#{max(0, trunc(target))}</span></p>\n")
    end)
    room
  end

end
