defmodule Systems.Command do
  use Systems.Reload

  def execute(player, command, arguments) do
    character = Characters.find_by_player(player)
    Components.Idle.value(character, 0)
    display_prompt(character)

    case Systems.Match.first(Commands.all, :keyword_starts_with, command) do
      nil ->
        Components.Player.send_message(character, ["scroll", "<p>What?</p>"])
      match ->
        :"Elixir.Commands.#{Inflex.camelize(Components.Name.value(match))}".execute(character, arguments)
    end
  end

  def display_prompt(character) do
    Components.Player.send_message(character, ["disable", "#prompt"])
    Components.Player.send_message(character, ["disable", "#command"])
    if Entity.has_component?(character, Components.HP) do
      Components.Player.send_message(character, ["scroll", "<p><span id='prompt'>[HP=#{Components.HP.value(character)}]:</span><input id='command' size='50' class='prompt'></input></p>"])
    else
      Components.Player.send_message(character, ["scroll", "<p><span id='prompt'>[#{Systems.Trainer.total_power(character)}]:</span><input id='command' size='50' class='prompt'></input></p>"])
    end
    Components.Player.send_message(character, ["focus", "#command"])
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      @after_compile Systems.Command

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
      end
    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      {:ok, command} = Entity.init
      Entity.add_component(command, Components.Keywords, __MODULE__.keywords)
      Entity.add_component(command, Components.Name, __MODULE__.name)
      Commands.add(command)
    end
  end

end
