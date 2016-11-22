defmodule ApathyDrive.Commands.Class do
  use ApathyDrive.Command
  alias ApathyDrive.Class

  def keywords do
    ApathyDrive.Class.names |> Enum.map(&String.downcase/1)
  end

  def execute(%Room{} = room, %Character{class: class} = character, args) do
    class = String.downcase(class)
    message =
      args
      |> Enum.join(" ")
      |> Character.sanitize()
    ApathyDrive.Endpoint.broadcast!("chat:#{class}", "scroll", %{html: "<p>[<span class='blue'>#{class}</span> : #{character.name}] #{message}</p>"})
    room
  end

end
