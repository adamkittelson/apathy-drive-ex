defmodule ApathyDrive.Commands.Absorb do
  use ApathyDrive.Command
  alias ApathyDrive.{Match, Repo}

  def keywords, do: ["absorb", "disintegrate"]

  def execute(%Mobile{spirit: %Spirit{inventory: inventory}} = mobile, item_name) do
    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:name_contains, item_name)

    case item do
      nil ->
        Mobile.send_scroll(mobile, "<p>You don't see \"#{item_name}\" here.</p>")
      %{item: item} ->
        mobile =
          put_in(mobile.spirit.inventory, List.delete(inventory, item))

        Repo.save!(mobile.spirit)

        exp = ApathyDrive.Item.deconstruction_experience(item)
        Mobile.send_scroll(mobile, "<p>You disintegrate the #{item["name"]} and absorb #{exp} essence.</p>")
        Mobile.add_experience(mobile, exp)
    end
  end

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Absorb what?</p>")
  end
  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    Mobile.absorb(mobile, item)
  end

end
