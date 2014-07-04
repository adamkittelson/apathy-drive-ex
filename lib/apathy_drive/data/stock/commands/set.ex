defmodule Commands.Set do
  use Systems.Command
  import Systems.Text

  def keywords, do: ["set"]

  def execute(entity, arguments) do

    case arguments do
      ["name", name] ->
        if Entity.has_component?(entity, Components.Name) do
          send_message(entity, "scroll", "<p>Not so fast, #{Components.Name.value(entity)}, you already have a name.</p>")
        else
          if Regex.match?(~r/[^a-zA-Z]/, name) do
            send_message(entity, "scroll", "<p>Your name must consist only of upper or lower case letters.</p>")
          else
            Entity.add_component(entity, Components.Name, capitalize_first(name))
            Components.Hints.deactivate(entity, "name")
            Entities.save!(entity)
            send_message(entity, "scroll", "<p>Your name has been set.</p>")
          end
        end
      ["name" | args] ->
        send_message(entity, "scroll", "<p>Your name must consist only of upper or lower case letters.</p>")
      _ ->
        send_message(entity, "scroll", "<p>I don't recognize that setting.</p>")
    end
  end

end
