defmodule ApathyDrive.Script do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Mobile, Room, RoomServer}

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

  def execute(%Room{} = room, %Mobile{} = mobile, scripts) do
    result =
      scripts
      |> Enum.find_value(fn(script) ->
           execute_script(room, mobile, script)
         end)
    result || room
  end

  def execute_script(%Room{} = room, %Mobile{} = mobile, [instruction | rest]) when is_list(instruction) do
    if room = execute_script(room, mobile, instruction) do
      room
    else
      execute_script(room, mobile, rest)
    end
  end

  def execute_script(%Room{} = room, %Mobile{} = mobile, [instruction | script]) do
    execute_instruction(room, mobile, instruction, script)
  end

  def execute_script(%Room{} = room, _mobile, []),   do: room
  def execute_script(_room, _mobile, nil), do: false
  def execute_script(%Room{} = room, %Mobile{} = mobile, script_id) when is_integer(script_id) do
    script =
      script_id
      |> find()

    execute_script(room, mobile, script)
  end

  def execute_script(%Room{} = room, %Mobile{} = mobile, instruction), do: execute_instruction(room, mobile, instruction, nil)

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"message" => %{"user" => user_message, "spectator" => spectator_message}}, script) do
    Mobile.send_scroll(mobile, "<p>#{user_message}</p>")
    Room.send_scroll(room, "<p>#{ApathyDrive.Text.interpolate(spectator_message, %{"user" => mobile})}</p>", mobile)
    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"message" => message},  script) do
    Mobile.send_scroll(mobile, "<p>#{message}</p>")
    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: nil}, %{"fail_flag" => %{"failure_message" => _message, "flag" => _flag}}, _script) do
    room
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: spirit} = mobile, %{"fail_flag" => %{"failure_message" => message, "flag" => flag}}, script) do
    if Map.has_key?(spirit.flags, flag) do
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{message}</p>")
      room
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: nil}, %{"give_flag" => %{"flag" => _flag, "value" => _value}}, _script) do
    room
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"give_flag" => %{"flag" => flag, "value" => value}}, script) do
    mobile =
      put_in(mobile.spirit.flags[flag], value)
      |> Mobile.save

    room = put_in(room.mobiles[mobile.ref], mobile)

    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: nil}, %{"flag_equals" => %{"flag" => _flag, "value" => _value}}, _script) do
    room
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: spirit} = mobile, %{"flag_equals" => %{"flag" => flag, "value" => value}}, script) do
    if value == spirit.flags[flag] do
      execute_script(room, mobile, script)
    else
      room
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: nil}, %{"flag_at_least" => %{"flag" => _flag, "value" => _value}}, _script) do
    room
  end

  def execute_instruction(room, mobile, %{"flag_at_least" => %{"flag" => flag, "value" => value}}, script) do
    if mobile.spirit.flags[flag] && mobile.spirit.flags[flag] >= value do
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(room, mobile, %{"script_link" => new_script}, script) do
    execute_script(room, mobile, [new_script | script])
  end

  def execute_instruction(%Room{} = room, %Mobile{alignment: alignment} = mobile, %{"allowed_alignments" => %{"failure_message" => message, "alignments" => alignments}}, script) do
    if alignment in alignments do
      execute_script(room, mobile, script)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{message}</p>")
      room
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"check_item" => %{"failure_message" => _message, "item" => _item_template_id}}, script) do
    execute_script(room, mobile, script)
    # if Mobile.has_item?(mobile, item_template_id) do
    #   execute_script(room, mobile, script)
    # else
    #   Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{message}</p>")
    #   room
    # end
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"take_item" => %{"failure_message" => message, "item" => item_template_id}}, script) do
    if mobile = Mobile.remove_item?(mobile, item_template_id) do
      Spirit.save(mobile.spirit)
      execute_script(room, mobile, script)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{message}</p>")
      room
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{delayed: false} = mobile, %{"add_delay" => delay}, script) do
    mobile =
      mobile
      |> ApathyDrive.TimerManager.send_after({:delay_execute_script, delay * 1000, {:execute_script, mobile.ref, script}})
      |> Map.put(:delayed, true)

    room = put_in(room.mobiles[mobile.ref], mobile)
    execute_script(room, mobile, [])
  end

  def execute_instruction(%Room{} = room, %Mobile{delayed: true} = mobile, %{"add_delay" => _delay}, _script) do
    execute_script(room, mobile, [])
  end

  def execute_instruction(%Room{} = room, %Mobile{hp: hp} = mobile, %{"check_hitpoints" => %{"modifier" => amount, "failure_script" => failure_script}}, script) do
    if hp < :rand.uniform(100 + amount) do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"check_stat" => %{"stat" => "strength", "modifier" => amount, "failure_script" => failure_script}}, script) do
    if Mobile.strength(mobile) < (:rand.uniform(100) + amount) do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"check_stat" => %{"stat" => "agility", "modifier" => amount, "failure_script" => failure_script}}, script) do
    if Mobile.agility(mobile) < (:rand.uniform(100) + amount) do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"check_stat" => %{"stat" => "intellect", "modifier" => amount, "failure_script" => failure_script}}, script) do
    if Mobile.will(mobile) < (:rand.uniform(100) + amount) do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"check_stat" => %{"stat" => "wisdom", "modifier" => amount, "failure_script" => failure_script}}, script) do
    if Mobile.will(mobile) < (:rand.uniform(100) + amount) do
      execute_script(room, mobile, failure_script)
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"remote_action" => %{"direction" => direction, "message" => message, "room_id" => room_id}}, script) do
    room = Room.initiate_remote_action(room, mobile, %{"destination" => room_id, "message" => message, "direction" => direction}, open_remotely: true)

    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"no_monsters" => _}, script) do
    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"give_item" => item_template_id}, script) do
    item = ApathyDrive.Item.generate_item(%{item_id: item_template_id, level: mobile.level})

    mobile =
      put_in(mobile.spirit.inventory, [item | mobile.spirit.inventory])

    Repo.save!(mobile.spirit)

    room = put_in(room.mobiles[mobile.ref], mobile)

    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"spawn_monster" => monster_template_id}, script) do
    monster =
      monster_template_id
      |> MonsterTemplate.create_monster(room)
      |> Mobile.init

    Room.audible_movement(room, nil)

    Room.display_enter_message(room, monster)

    room = put_in(room.mobiles[monster.ref], monster)

    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"min_level" => %{"failure_message" => message, "level" => level}}, script) do
    if mobile.level < level do
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{message}</p>")
      room
    else
      execute_script(room, mobile, script)
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: %Spirit{} = spirit} = mobile, %{"add_experience" => exp}, script) do
    spirit = Spirit.add_experience(spirit, exp)
    mobile = Map.put(mobile, :spirit, spirit)
    room = put_in(room.mobiles[mobile.ref], mobile)
    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"cast_ability" => ability_id}, script) do
    case ApathyDrive.Ability.find(ability_id) do
      nil ->
        Mobile.send_scroll(mobile, "<p><span class='red'>Not Implemented: Ability ##{ability_id}</span></p>")
      ability ->
        ability = Map.put(ability, "ignores_global_cooldown", true)

        send(self, {:execute_ability, %{caster: mobile.ref, ability: ability, target: [mobile]}})
    end
    execute_script(room, mobile, script)
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"room_item" => %{"failure_message" => failure_message, "item" => item_name}}, script) do
    if Room.find_item(room, item_name) do
      execute_script(room, mobile, script)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{failure_message}</p>")
      room
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: nil} = mobile, %{"price" => %{"failure_message" => failure_message, "price_in_copper" => _price}}, _script) do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{failure_message}</p>")
    room
  end

  def execute_instruction(%Room{} = room, %Mobile{spirit: %Spirit{experience: exp} = spirit} = mobile, %{"price" => %{"failure_message" => failure_message, "price_in_copper" => price}}, script) do
    if exp >= price do
      spirit = Spirit.add_experience(spirit, -price)
      mobile = Map.put(mobile, :spirit, spirit)
      room = put_in(room.mobiles[mobile.ref], mobile)
      execute_script(room, mobile, script)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{failure_message}</p>")
      room
    end
  end

  def execute_instruction(room, mobile, %{"random" => scripts}, script) do
    roll = :rand.uniform(100)

    number =
      scripts
      |> Map.keys
      |> Enum.map(&String.to_integer/1)
      |> Enum.sort
      |> Enum.find(&(&1 >= roll))

    if script do
      send(self, {:execute_script, mobile.ref, script})
    end

    case scripts["#{number}"] do
      script_id when is_integer(script_id) ->
        found = find(script_id)

        execute_script(room, mobile, found)
      instructions ->
        execute_script(room, mobile, instructions)
    end
  end

  def execute_instruction(%Room{} = room, %Mobile{} = mobile, %{"teleport" => room_id}, script) do
    room_exit =
      %{
        "kind" => "Action",
        "destination" => room_id,
        "mover_message" => "<span class='blue'>You vanish into thin air and reappear somewhere else!</span>",
        "from_message" => "<span class='blue'>{{Name}} vanishes into thin air!</span>",
        "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
      }

    room = ApathyDrive.Commands.Move.execute(room, mobile, room_exit)

    room_id
    |> RoomServer.find
    |> send({:execute_script, mobile.ref, script})

    room
  end

  def execute_instruction(room, mobile, instruction, _script) do
    Mobile.send_scroll(mobile, "<p><span class='red'>Not Implemented: #{inspect instruction}</span></p>")
    execute_script(room, mobile, [])
  end

end
