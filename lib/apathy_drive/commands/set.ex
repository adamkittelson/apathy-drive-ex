defmodule Commands.Set do
  use ApathyDrive.Command
  import Systems.Text

  def keywords, do: ["set"]

  def execute(%Spirit{name: nil} = spirit, ["name", name]) do
    if Regex.match?(~r/[^a-zA-Z]/, name) do
      Spirit.send_scroll(spirit, "<p>Your name must consist only of upper or lower case letters.</p>")
    else
      spirit
      |> Map.put(:name, capitalize_first(name))
      |> Spirit.deactivate_hint("name")
      |> Spirit.save
      |> Spirit.send_scroll("<p>Your name has been set.</p>")
    end
  end

  def execute(%Spirit{name: nil} = spirit, ["name" | _args]) do
    spirit
    |> Spirit.send_scroll("<p>Your name must consist only of upper or lower case letters.</p>")
  end

  def execute(%Spirit{name: name} = spirit, ["name", _]) do
    spirit
    |> Spirit.send_scroll("<p>Not so fast, #{name}, you already have a name.</p>")
  end

  def execute(%Spirit{} = spirit, _args) do
    spirit
    |> Spirit.send_scroll("<p>I don't recognize that setting.</p>")
  end

end
