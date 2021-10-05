defmodule ApathyDrive.Commands.Gems do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Repo}
  require Ecto.Query

  @gems %{
    "ruby" => "red",
    "sapphire" => "blue",
    "topaz" => "dark-yellow",
    "emerald" => "green",
    "diamond" => "white",
    "amethyst" => "magenta",
    "skull" => "dark-grey"
  }
  @grades ["chipped", "flawed", "", "flawless", "perfect"]

  def keywords, do: ["gems", "gem"]

  def execute(
        %Room{} = room,
        %Character{} = character,
        _args
      ) do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>You have the following gems:</span> <span class='dark-cyan'>(</span>chipped<span class='dark-cyan'>/</span>flawed<span class='dark-cyan'>/</span>normal<span class='dark-cyan'>/</span>flawless<span class='dark-cyan'>/</span>perfect<span class='dark-cyan'>)</span></p>"
    )

    items =
      character
      |> Ecto.assoc(:characters_items)
      |> Ecto.Query.preload([:item])
      |> Repo.all()
      |> Enum.filter(fn ci ->
        @gems
        |> Map.keys()
        |> Enum.any?(&String.contains?(ci.item.name, &1))
      end)

    @gems
    |> Map.keys()
    |> Enum.each(fn gem ->
      Mobile.send_scroll(character, "<p>  <span class='#{@gems[gem]}'>#{gem}<span></p>")

      grades =
        @grades
        |> Enum.map(fn grade ->
          name =
            [grade, gem]
            |> Enum.join(" ")
            |> String.trim()

          if item = Enum.find(items, &(&1.item.name == name)) do
            item.count
          else
            0
          end
        end)
        |> Enum.join("<span class='dark-cyan'>/</span>")

      Mobile.send_scroll(character, "<p>    #{grades}</p>")
    end)

    room
  end
end
