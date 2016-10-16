defmodule ApathyDrive.Script do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Monster, MonsterTemplate, Room, RoomServer}

  schema "scripts" do
    field :instructions, ApathyDrive.JSONB, default: []

    timestamps
  end

  def changeset(script, params \\ %{}) do
    script
    |> cast(params, ~w(instructions), ~w())
  end

  def find(id) do
    ApathyDrive.Repo.get(__MODULE__, id)
    |> Map.get(:instructions)
  end

  def execute(%Room{} = room, %Monster{} = monster, scripts) do
    result =
      scripts
      |> Enum.find_value(fn(script) ->
           execute_script(room, monster, script)
         end)
    result || room
  end

  def execute_script(%Room{} = room, %Monster{} = monster, [instruction | rest]) when is_list(instruction) do
    if room = execute_script(room, monster, instruction) do
      room
    else
      execute_script(room, monster, rest)
    end
  end

  def execute_script(%Room{} = room, %Monster{} = monster, [instruction | script]) do
    execute_instruction(room, monster, instruction, script)
  end

  def execute_script(%Room{} = room, _monster, []),   do: room
  def execute_script(_room, _monster, nil), do: false
  def execute_script(%Room{} = room, %Monster{} = monster, script_id) when is_integer(script_id) do
    script =
      script_id
      |> find()

    execute_script(room, monster, script)
  end

  def execute_script(%Room{} = room, %Monster{} = monster, instruction), do: execute_instruction(room, monster, instruction, nil)

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"message" => %{"user" => user_message, "spectator" => spectator_message}}, script) do
    Monster.send_scroll(monster, "<p>#{user_message}</p>")
    Room.send_scroll(room, "<p>#{ApathyDrive.Text.interpolate(spectator_message, %{"user" => monster})}</p>", monster)
    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"message" => message},  script) do
    Monster.send_scroll(monster, "<p>#{message}</p>")
    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: nil}, %{"fail_flag" => %{"failure_message" => _message, "flag" => _flag}}, _script) do
    room
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: spirit} = monster, %{"fail_flag" => %{"failure_message" => message, "flag" => flag}}, script) do
    if Map.has_key?(spirit.flags, flag) do
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
      room
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: nil}, %{"give_flag" => %{"flag" => _flag, "value" => _value}}, _script) do
    room
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"give_flag" => %{"flag" => flag, "value" => value}}, script) do
    monster =
      put_in(monster.spirit.flags[flag], value)
      |> Monster.save

    room = put_in(room.monsters[monster.ref], monster)

    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: nil}, %{"flag_equals" => %{"flag" => _flag, "value" => _value}}, _script) do
    room
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: spirit} = monster, %{"flag_equals" => %{"flag" => flag, "value" => value}}, script) do
    if value == spirit.flags[flag] do
      execute_script(room, monster, script)
    else
      room
    end
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: nil}, %{"flag_at_least" => %{"flag" => _flag, "value" => _value}}, _script) do
    room
  end

  def execute_instruction(room, monster, %{"flag_at_least" => %{"flag" => flag, "value" => value}}, script) do
    if monster.spirit.flags[flag] && monster.spirit.flags[flag] >= value do
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(room, monster, %{"script_link" => new_script}, script) do
    execute_script(room, monster, [new_script | script])
  end

  def execute_instruction(%Room{} = room, %Monster{alignment: alignment} = monster, %{"allowed_alignments" => %{"failure_message" => message, "alignments" => alignments}}, script) do
    if alignment in alignments do
      execute_script(room, monster, script)
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
      room
    end
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"check_item" => %{"failure_message" => _message, "item" => _item_template_id}}, script) do
    execute_script(room, monster, script)
    # if Monster.has_item?(monster, item_template_id) do
    #   execute_script(room, monster, script)
    # else
    #   Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
    #   room
    # end
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"take_item" => %{"failure_message" => message, "item" => item_template_id}}, script) do
    if monster = Monster.remove_item?(monster, item_template_id) do
      Monster.save(monster)
      execute_script(room, monster, script)
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
      room
    end
  end

  def execute_instruction(%Room{} = room, %Monster{delayed: false} = monster, %{"add_delay" => delay}, script) do
    monster =
      monster
      |> ApathyDrive.TimerManager.send_after({:delay_execute_script, delay * 1000, {:execute_script, monster.ref, script}})
      |> Map.put(:delayed, true)

    room = put_in(room.monsters[monster.ref], monster)
    execute_script(room, monster, [])
  end

  def execute_instruction(%Room{} = room, %Monster{delayed: true} = monster, %{"add_delay" => _delay}, _script) do
    execute_script(room, monster, [])
  end

  def execute_instruction(%Room{} = room, %Monster{hp: hp} = monster, %{"check_hitpoints" => %{"modifier" => amount, "failure_script" => failure_script}}, script) do
    if hp < :rand.uniform(100 + amount) do
      execute_script(room, monster, failure_script)
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"check_stat" => %{"stat" => "strength", "modifier" => amount, "failure_script" => failure_script}}, script) do
    if Monster.strength(monster) < (:rand.uniform(100) + amount) do
      execute_script(room, monster, failure_script)
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"check_stat" => %{"stat" => "agility", "modifier" => amount, "failure_script" => failure_script}}, script) do
    if Monster.agility(monster) < (:rand.uniform(100) + amount) do
      execute_script(room, monster, failure_script)
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"check_stat" => %{"stat" => "intellect", "modifier" => amount, "failure_script" => failure_script}}, script) do
    if Monster.will(monster) < (:rand.uniform(100) + amount) do
      execute_script(room, monster, failure_script)
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"check_stat" => %{"stat" => "wisdom", "modifier" => amount, "failure_script" => failure_script}}, script) do
    if Monster.will(monster) < (:rand.uniform(100) + amount) do
      execute_script(room, monster, failure_script)
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"remote_action" => %{"direction" => direction, "message" => message, "room_id" => room_id}}, script) do
    room = Room.initiate_remote_action(room, monster, %{"destination" => room_id, "message" => message, "direction" => direction}, open_remotely: true)

    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"no_monsters" => _}, script) do
    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"give_item" => item_template_id}, script) do
    item = ApathyDrive.Item.generate_item(%{item_id: item_template_id, level: monster.level})

    monster =
      put_in(monster.spirit.inventory, [item | monster.spirit.inventory])

    Repo.save!(monster.spirit)

    room = put_in(room.monsters[monster.ref], monster)

    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"spawn_monster" => monster_template_id}, script) do
    monster =
      monster_template_id
      |> MonsterTemplate.create_monster(room)
      |> Monster.init

    room = Room.monster_entered(room, monster)

    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"min_level" => %{"failure_message" => message, "level" => level}}, script) do
    if monster.level < level do
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
      room
    else
      execute_script(room, monster, script)
    end
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: %Spirit{}} = monster, %{"add_experience" => exp}, script) do
    monster = Spirit.add_experience(monster, exp)
    room = put_in(room.monsters[monster.ref], monster)
    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"cast_ability" => ability_id}, script) do
    case ApathyDrive.Ability.find(ability_id) do
      nil ->
        Monster.send_scroll(monster, "<p><span class='red'>Not Implemented: Ability ##{ability_id}</span></p>")
      ability ->
        ability = Map.put(ability, "ignores_global_cooldown", true)

        send(self, {:execute_ability, %{caster: monster.ref, ability: ability, target: [monster]}})
    end
    execute_script(room, monster, script)
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"room_item" => %{"failure_message" => failure_message, "item" => item_name}}, script) do
    if Room.find_item(room, item_name) do
      execute_script(room, monster, script)
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{failure_message}</p>")
      room
    end
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: nil} = monster, %{"price" => %{"failure_message" => failure_message, "price_in_copper" => _price}}, _script) do
    Monster.send_scroll(monster, "<p><span class='dark-green'>#{failure_message}</p>")
    room
  end

  def execute_instruction(%Room{} = room, %Monster{spirit: %Spirit{experience: exp}} = monster, %{"price" => %{"failure_message" => failure_message, "price_in_copper" => price}}, script) do
    if exp >= price do
      monster = Spirit.add_experience(monster, -price)
      room = put_in(room.monsters[monster.ref], monster)
      execute_script(room, monster, script)
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{failure_message}</p>")
      room
    end
  end

  def execute_instruction(room, monster, %{"random" => scripts}, script) do
    roll = :rand.uniform(100)

    number =
      scripts
      |> Map.keys
      |> Enum.map(&String.to_integer/1)
      |> Enum.sort
      |> Enum.find(&(&1 >= roll))

    if script do
      send(self, {:execute_script, monster.ref, script})
    end

    case scripts["#{number}"] do
      script_id when is_integer(script_id) ->
        found = find(script_id)

        execute_script(room, monster, found)
      instructions ->
        execute_script(room, monster, instructions)
    end
  end

  def execute_instruction(%Room{} = room, %Monster{} = monster, %{"teleport" => room_id}, script) do
    room_exit =
      %{
        "kind" => "Action",
        "destination" => room_id,
        "mover_message" => "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
        "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
        "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
      }

    room = ApathyDrive.Commands.Move.execute(room, monster, room_exit)

    room_id
    |> RoomServer.find
    |> send({:execute_script, monster.ref, script})

    room
  end

  def execute_instruction(room, monster, instruction, _script) do
    Monster.send_scroll(monster, "<p><span class='red'>Not Implemented: #{inspect instruction}</span></p>")
    execute_script(room, monster, [])
  end

end
