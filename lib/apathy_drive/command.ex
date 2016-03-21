defmodule ApathyDrive.Command do
  defstruct name: nil, keywords: nil, module: nil
  require Logger
  alias ApathyDrive.{Commands, Mobile, Match}

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def all do
    [Commands.Abilities, Commands.Absorb, Commands.Ask, Commands.Attack,
     Commands.Bash, Commands.Class, Commands.Close, Commands.Construct,
     Commands.Cooldowns, Commands.Drop, Commands.Experience, Commands.Forms,
     Commands.Get, Commands.Gossip, Commands.Greet,
     Commands.Inventory, Commands.List, Commands.Lock, Commands.Look, Commands.Open,
     Commands.Possess, Commands.Protection, Commands.Remove,
     Commands.Say, Commands.Score, Commands.Search,
     Commands.Unpossess, Commands.Wear, Commands.Who, Commands.Turn, Commands.Purify]
  end

  def execute(%Mobile{room_id: room_id}, command, arguments) do
    room_id
    |> Room.find
    |> Room.execute_command(self, command, arguments)
  end

  def execute(%Room{} = room, mobile, command, arguments) do
    full_command = Enum.join([command | arguments], " ")

    cond do
      command in @directions ->
        Commands.Move.execute(room, mobile, command)
      scripts = Room.command(room, full_command) ->
        Mobile.execute_room_command(mobile, scripts)
      command_exit = Room.command_exit(room, full_command) ->
        Commands.Move.execute(room, mobile, Map.put(command_exit, "kind", "Action"), nil)
      remote_action_exit = Room.remote_action_exit(room, full_command) ->
        Mobile.trigger_remote_action(mobile, self, remote_action_exit)
      cmd = Match.one(Enum.map(all, &(&1.to_struct)), :keyword_starts_with, command) ->
        case cmd do
          %ApathyDrive.Command{name: "look"} ->
            Mobile.look(mobile, arguments)
          %ApathyDrive.Command{name: "search"} ->
            Commands.Search.execute(room, mobile, arguments)
          _ ->
            cmd.module.execute(mobile, arguments)
        end
      true ->
        Mobile.use_ability(mobile, command, arguments)
    end
  end

  defmacro __using__(_opts) do
    quote do
      import Systems.Text
      alias ApathyDrive.Mobile

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
      end

      def to_struct do
        %ApathyDrive.Command{name: name, keywords: keywords, module: __MODULE__}
      end
    end
  end

end
