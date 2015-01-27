defmodule Commands.Set do
  use Systems.Command
  import Systems.Text

  def keywords, do: ["set"]

  def execute(spirit, monster, arguments) do

    case arguments do
      ["name", name] ->
        if Entity.has_component?(spirit, Components.Name) do
          send_message(spirit, "scroll", "<p>Not so fast, #{Components.Name.value(spirit)}, you already have a name.</p>")
        else
          if Regex.match?(~r/[^a-zA-Z]/, name) do
            send_message(spirit, "scroll", "<p>Your name must consist only of upper or lower case letters.</p>")
          else
            Entity.add_component(spirit, Components.Name, capitalize_first(name))
            #Spirit.deactivate_hint(spirit, "name")
            Entities.save!(spirit)
            send_message(spirit, "scroll", "<p>Your name has been set.</p>")
          end
        end
      ["name" | _args] ->
        send_message(spirit, "scroll", "<p>Your name must consist only of upper or lower case letters.</p>")
      _ ->
        send_message(spirit, "scroll", "<p>I don't recognize that setting.</p>")
    end
  end

end
