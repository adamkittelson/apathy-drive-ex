defmodule ApathyDrive.Commands.Class do
  use ApathyDrive.Command
  alias ApathyDrive.Class

  def keywords do
    ApathyDrive.Class.names |> Enum.map(&String.downcase/1)
  end

  def execute(%Room{} = room, %Monster{spirit: %Spirit{class: %Class{name: class}}} = monster, args) do
    class = String.downcase(class)
    message =
      args
      |> Enum.join(" ")
      |> Monster.sanitize()
    ApathyDrive.Endpoint.broadcast!("chat:#{class}", "scroll", %{html: "<p>[<span class='#{color(class)}'>#{class}</span> : #{Monster.aligned_spirit_name(monster)}] #{message}</p>"})
    room
  end

  defp color("angel"), do: "white"
  defp color("demon"), do: "magenta"
  defp color("elemental"), do: "dark-cyan"
end
