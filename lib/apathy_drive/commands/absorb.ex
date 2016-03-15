defmodule ApathyDrive.Commands.Absorb do
  use ApathyDrive.Command

  def keywords, do: ["absorb", "disintegrate"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Absorb what?</p>")
  end
  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    room =
      mobile
      |> Mobile.room_id
      |> Room.find

    case Room.destroy_item(room, item) do
      {:ok, %{} = item} ->
        success(mobile, item)
      {:cant_destroy, item_name} ->
        Mobile.send_scroll(mobile, "<p>#{item_name |> capitalize_first} cannot be absorbed.</p>")
      :not_found ->
        case Mobile.destroy_item(mobile, item) do
          {:ok, %{} = item} ->
            success(mobile, item)
          :not_found ->
            Mobile.send_scroll(mobile, "<p>You don't see \"#{item}\" here.</p>")
        end
    end
  end

  def success(mobile, %{"name" => name} = item) do
    exp = ApathyDrive.Item.deconstruction_experience(item)
    Mobile.send_scroll(mobile, "<p>You disintegrate the #{name} and absorb #{exp} essence.</p>")
    Mobile.add_experience(mobile, exp)
    if :rand.uniform(10) == 10 do
      Mobile.add_form(mobile, item)
    end
  end
end
