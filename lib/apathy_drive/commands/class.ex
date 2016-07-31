defmodule ApathyDrive.Commands.Class do
  use ApathyDrive.Command
  alias ApathyDrive.Class

  def keywords do
    ApathyDrive.Class.names |> Enum.map(&String.downcase/1)
  end

  def execute(%Room{} = room, %Mobile{spirit: %Spirit{class: %Class{name: class}}} = mobile, message) do
    class = String.downcase(class)
    message = Mobile.sanitize(message)
    ApathyDrive.Endpoint.broadcast!("chat:#{class}", "scroll", %{html: "<p>[<span class='#{color(class)}'>#{class}</span> : #{Mobile.aligned_spirit_name(mobile)}] #{message}</p>"})
    room
  end

  defp color("angel"), do: "white"
  defp color("demon"), do: "magenta"
  defp color("elemental"), do: "dark-cyan"
end
