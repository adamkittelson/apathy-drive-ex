defmodule Commands.Say do
  use ApathyDrive.Command

  def keywords, do: ["say"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{room_id: room_id} = monster, arguments) do
    message = sanitize(arguments)

    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{room_id}", "scroll", %{:html => "<p>#{Systems.Text.capitalize_first(monster.name)} says: #{message}</p>"}

    Monster.send_scroll(monster, "<p>You say: #{message}</p>")
  end

  def sanitize(arguments) do
    {:safe, message} = arguments
                       |> Enum.join(" ")
                       |> Phoenix.HTML.html_escape

    "<span class='dark-green'>\"#{message}\"</span>"
  end

end
