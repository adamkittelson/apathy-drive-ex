defmodule Systems.Command do

  @aliases [ u: "up",
             d: "down",
             n: "north",
             ne: "northeast",
             e:  "east",
             se: "southeast",
             s:  "south",
             sw: "southwest",
             w:  "west",
             nw: "northwest" ]

  @directions [ "up",
                "down",
                "north",
                "northeast",
                "east",
                "southeast",
                "south",
                "southwest",
                "west",
                "northwest" ]

  def execute(player, command, arguments) do
    character = Components.Login.get_character(player)
    command = @aliases[:"#{command}"] || command

    current_room = Systems.Room.get_current_room(character)
    if current_room do
      exit_directions = Systems.Room.exit_directions(current_room)
    end

    display_prompt(character)

    cond do
      Enum.member? @directions, command ->
        if exit_directions && (Enum.member? exit_directions, command) do
          Systems.Room.move(character, command)
        else
          Players.send_message(player, ["scroll", "<p>There is no exit in that direction.</p>"])
        end
      Enum.member?(["l", "look"], command) ->
        if Enum.any? arguments do
          if target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) do
            Systems.Description.add_description_to_scroll(character, target)
          else
            Players.send_message(player, ["scroll", "<p>You do not notice that here.</p>"])
          end
        else
          Systems.Room.display_room_in_scroll(character, current_room)
        end
      Enum.member?(["i", "inventory"], command) ->
        Systems.Item.display_inventory(character)
      command == "list" ->
        Systems.Shop.list(character, current_room)
      command == "buy" ->
        Systems.Shop.buy(character, current_room, Enum.join(arguments, " "))
      command == "sell" ->
        Systems.Shop.sell(character, current_room, Enum.join(arguments, " "))
      command == "wear" ->
        Systems.Item.equip(character, Enum.join(arguments, " "))
      command == "remove" ->
        Systems.Item.unequip(character, Enum.join(arguments, " "))
      true ->
        case Systems.Match.first(Commands.all, :keyword_starts_with, command) do
          nil ->
            Players.send_message(player, ["scroll", "<p>What?</p>"])
          match ->
            :"Elixir.Commands.#{Inflex.camelize(Components.Name.value(match))}".execute(character, arguments)
        end
    end
  end

  def find_entity_in_room(room, string) do
    room |> Systems.Room.entities_in_room |> Systems.Match.first(:name_contains, string)
  end

  def display_prompt(character) do
    Components.Player.send_message(character, ["disable", "#prompt"])
    Components.Player.send_message(character, ["disable", "#command"])
    Components.Player.send_message(character, ["scroll", "<p><span id='prompt'>[HP=#{Components.HP.value(character)}]:</span><input id='command' class='prompt'></input></p>"])
    Components.Player.send_message(character, ["focus", "#command"])
  end

  defmacro __using__(_opts) do
    quote do
      @before_compile Systems.Command
      @after_compile Systems.Command

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> String.downcase
      end
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      Code.ensure_loaded(Commands)
      Commands.start_link
    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      Code.ensure_loaded(Entity)
      Code.ensure_loaded(Components.Keywords)
      Code.ensure_loaded(Components.Name)
      {:ok, command} = Entity.init
      Entity.add_component(command, Components.Keywords, __MODULE__.keywords)
      Entity.add_component(command, Components.Name, __MODULE__.name)
      Commands.add(command)
    end
  end

end
