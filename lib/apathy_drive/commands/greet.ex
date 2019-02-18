defmodule ApathyDrive.Commands.Greet do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Match, Monster}

  def keywords, do: ["greet"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Greet whom?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, arguments) do
    query =
      arguments
      |> Enum.join(" ")
      |> String.downcase()

    target =
      room.mobiles
      |> Map.values()
      |> Match.one(:name_contains, query)

    greet(room, character, target)
    room
  end

  def greet(%Room{}, %Character{} = greeter, nil) do
    Mobile.send_scroll(greeter, "<p>Greet whom?</p>")
  end

  def greet(%Room{} = room, %Character{} = greeter, %{} = target) do
    room.mobiles
    |> Enum.each(fn {_ref, mobile} ->
      cond do
        mobile == greeter ->
          Mobile.send_scroll(
            mobile,
            "<p><span class='dark-green'>You greet #{Mobile.colored_name(target)}.</span></p>"
          )

          if target.__struct__ == Monster do
            Mobile.send_scroll(
              mobile,
              "<p>#{target.greeting}</p>"
            )
          end

        mobile == target ->
          Mobile.send_scroll(
            mobile,
            "<p><span class='dark-green'>#{Mobile.colored_name(greeter)} greets you.</span></p>"
          )

        true ->
          Mobile.send_scroll(
            mobile,
            "<p><span class='dark-green'>#{Mobile.colored_name(greeter)} greets #{
              Mobile.colored_name(target)
            }.</span></p>"
          )
      end
    end)
  end
end
