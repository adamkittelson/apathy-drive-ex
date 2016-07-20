defmodule ApathyDrive.Command do
  defstruct name: nil, keywords: nil, module: nil
  require Logger
  alias ApathyDrive.{Commands, Mobile, Match, Room, RoomServer}

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def all do
    [Commands.Abilities, Commands.Absorb, Commands.Ask, Commands.Attack,
     Commands.Bash, Commands.Class, Commands.Close, Commands.Construct,
     Commands.Cooldowns, Commands.Delve, Commands.Drop, Commands.Experience,
     Commands.Forms, Commands.Get, Commands.Gossip, Commands.Greet,
     Commands.Inventory, Commands.List, Commands.Lock, Commands.Look, Commands.Open,
     Commands.Possess, Commands.Remove, Commands.Return,
     Commands.Say, Commands.Score, Commands.Search, Commands.System,
     Commands.Unpossess, Commands.Wear, Commands.Who]
  end

  def execute(%Room{} = room, mobile_ref, command, arguments) do
    full_command = Enum.join([command | arguments], " ")

    mobile = room.mobiles[mobile_ref]

    cond do
      command in @directions ->
        Commands.Move.execute(room, mobile, command)
      command_exit = Room.command_exit(room, full_command) ->
        Commands.Move.execute(room, mobile, Map.put(command_exit, "kind", "Action"))
      remote_action_exit = Room.remote_action_exit(room, full_command) ->
        Room.initiate_remote_action(room, mobile, remote_action_exit)
      scripts = Room.command(room, full_command) ->
        Mobile.execute_room_command(mobile, scripts)
      cmd = Match.one(Enum.map(all, &(&1.to_struct)), :keyword_starts_with, command) ->
        cmd.module.execute(room, mobile, arguments)
      true ->
        Mobile.use_ability(mobile, command, arguments)
    end
  end

  defmacro __using__(_opts) do
    quote do
      import ApathyDrive.Text
      alias ApathyDrive.{Mobile, Room, RoomServer}

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
