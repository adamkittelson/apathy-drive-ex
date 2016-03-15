defmodule ApathyDrive.Commands.Purify do
  use ApathyDrive.Command
  alias ApathyDrive.{Class, Mobile, World}

  def keywords, do: ["purify"]

  def execute(mobile, _arguments) do
    case World.mobile(mobile) do
      %Mobile{spirit: %Spirit{class: %Class{name: "Demon"}}} = demon ->
        Mobile.send_scroll(demon, "<p><span class='red'>Demons are incapable of purification!</span></p>")
      %Mobile{spirit: %Spirit{class: %Class{name: class}, experience: exp}} = angel when class in ["Angel", "Elemental"] ->
        essence_cost =
          case World.average_essence("angel") do
            nil ->
              div(exp, 100)
            essence ->
              div(essence, 100)
          end

        essence_cost = max(1000, essence_cost)

        if essence_cost < exp do
          Mobile.send_scroll(mobile, "<p>You infuse the room with #{essence_cost} essence.</p>")
          angel.room_id
          |> Room.find
          |> Room.purify(essence_cost)

          Mobile.add_experience(mobile, -essence_cost)
        else
          Mobile.send_scroll(mobile, "<p>You need at least #{essence_cost} to purify this room.</p>")
        end
    end

  end
end
