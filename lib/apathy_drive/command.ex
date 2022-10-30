defmodule ApathyDrive.Command do
  defstruct name: nil, keywords: nil, module: nil
  require Logger

  alias ApathyDrive.{
    Ability,
    Character,
    Commands,
    Emote,
    Monster,
    Match,
    Mobile,
    Room,
    RoomServer,
    TimerManager
  }

  @callback execute(%Room{}, %Monster{}, list) :: %Room{}

  @directions [
    "n",
    "north",
    "ne",
    "northeast",
    "e",
    "east",
    "se",
    "southeast",
    "s",
    "south",
    "sw",
    "southwest",
    "w",
    "west",
    "nw",
    "northwest",
    "u",
    "up",
    "d",
    "down"
  ]

  def all do
    [
      Commands.Abilities,
      Commands.Ask,
      Commands.Auto,
      Commands.Attack,
      Commands.Attune,
      Commands.Bash,
      Commands.Buy,
      Commands.Close,
      Commands.Cooldowns,
      Commands.Drop,
      Commands.Emote,
      Commands.Experience,
      Commands.Forget,
      Commands.Gems,
      Commands.Get,
      Commands.Give,
      Commands.Go,
      Commands.Gossip,
      Commands.Greet,
      Commands.Help,
      Commands.Inventory,
      Commands.Join,
      Commands.Invite,
      Commands.Leave,
      Commands.List,
      Commands.Look,
      Commands.Open,
      Commands.Party,
      Commands.Pick,
      Commands.Possess,
      Commands.Protection,
      Commands.Read,
      Commands.Remove,
      Commands.Reply,
      Commands.Rest,
      Commands.Say,
      Commands.Search,
      Commands.Sell,
      Commands.Set,
      Commands.Skills,
      Commands.Sneak,
      Commands.Status,
      Commands.System,
      Commands.Teleport,
      Commands.Tell,
      Commands.Top,
      Commands.Train,
      Commands.Untrain,
      Commands.Use,
      Commands.Wear,
      Commands.Who
    ]
  end

  def enqueue(%Room{} = room, mobile_ref, command, arguments) do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      Logger.info("Enqueueing command for #{mobile.name}: #{inspect({command, arguments})}")

      if :queue.len(mobile.commands) > 10 do
        Mobile.send_scroll(mobile, "<p>Why don't you slow down for a few seconds?</p>")
        mobile
      else
        update_in(mobile.commands, &:queue.in({command, arguments}, &1))
        |> TimerManager.send_after({:execute_command, 0, {:execute_command, mobile_ref}})
      end
    end)
  end

  def enqueue_ability(%Room{} = room, mobile_ref, command, arguments) do
    Room.update_mobile(room, mobile_ref, fn _room, mobile ->
      Logger.info("Enqueueing command for #{mobile.name}: #{inspect({command, arguments})}")

      case :queue.out(mobile.commands) do
        {{:value, {^command, ^arguments}}, _commands} ->
          mobile

        _ ->
          update_in(mobile.commands, &:queue.in({command, arguments}, &1))
          |> TimerManager.send_after({:execute_command, 0, {:execute_command, mobile_ref}})
      end
    end)
  end

  def execute(%Room{} = room, monster_ref, command, arguments) do
    full_command = Enum.join([command | arguments], " ")

    room =
      Room.update_mobile(room, monster_ref, fn _room, mobile ->
        Map.put(mobile, :command, {command, arguments})
      end)

    monster = room.mobiles[monster_ref]

    Logger.info("#{monster && monster.name} executing command: #{full_command}")

    {time, response} =
      :timer.tc(fn ->
        cond do
          is_nil(monster) ->
            {:error, :not_here, room}

          command in @directions ->
            Commands.Move.execute(room, monster, command)

          command_exit = Room.command_exit(room, full_command) ->
            Commands.Move.execute(
              room,
              monster,
              Map.put(command_exit, "kind", "Action")
            )

          remote_action_exit = Room.remote_action_exit(room, full_command) ->
            Room.initiate_remote_action(room, monster, remote_action_exit)

          scripts = Room.command(room, full_command) ->
            execute_room_command(room, monster, scripts)

          "bash" == String.downcase(command) ->
            bash = Match.one(Enum.map(all(), & &1.to_struct), :keyword_starts_with, command)
            bash.module.execute(room, monster, arguments)

          skill = monster.skills[String.downcase(command)] ->
            ability = skill.module.ability(monster)
            Ability.execute(room, monster.ref, ability, Enum.join(arguments, " "))

          ability = monster.abilities[String.downcase(command)] ->
            Ability.execute(room, monster.ref, ability, Enum.join(arguments, " "))

          true ->
            case Match.all(Enum.map(all(), & &1.to_struct), :keyword_starts_with, command) do
              %__MODULE__{} = cmd ->
                cmd.module.execute(room, monster, arguments)

              nil ->
                if emote = Emote.match_by_name(command, arguments) do
                  target = Enum.join(arguments, " ")
                  Emote.execute(emote, room, monster, target)

                  room
                else
                  Mobile.send_scroll(monster, "<p>Your command had no effect.</p>")
                  room
                end

              commands ->
                Mobile.send_scroll(
                  monster,
                  "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
                )

                Enum.each(commands, fn command ->
                  Mobile.send_scroll(
                    monster,
                    "<p>-- #{command.name}</p>"
                  )
                end)

                room
            end
        end
      end)

    if match?(%Room{}, response) do
      Logger.info(
        "#{full_command} executed for #{monster && monster.name} in #{time / 1000 / 1000} seconds"
      )
    end

    response
  end

  defp execute_room_command(room, monster, scripts) when is_list(scripts) do
    if Mobile.confused(monster, room) do
      room
    else
      scripts = Enum.map(scripts, &ApathyDrive.Script.find/1)
      ApathyDrive.Script.execute(room, monster, scripts)
    end
  end

  defp execute_room_command(room, monster, script) when is_binary(script) do
    if Mobile.confused(monster, room) do
      room
    else
      Module.safe_concat([ApathyDrive, Scripts, Macro.camelize(script)]).execute(
        room,
        monster.ref,
        monster.ref
      )
    end
  end

  defmacro __using__(_opts) do
    quote do
      import ApathyDrive.Text
      alias ApathyDrive.{Character, Mobile, Room, RoomServer}

      @behaviour ApathyDrive.Command

      def name do
        __MODULE__
        |> Atom.to_string()
        |> String.split(".")
        |> List.last()
        |> Inflex.underscore()
      end

      def to_struct do
        %ApathyDrive.Command{name: name(), keywords: keywords(), module: __MODULE__}
      end
    end
  end
end
