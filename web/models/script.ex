defmodule ApathyDrive.Script do
  alias ApathyDrive.Mobile
  use ApathyDrive.Web, :model

  schema "scripts" do
    field :instructions, ApathyDrive.JSONB, default: []

    timestamps
  end

  def find(id) do
    ApathyDrive.Repo.get(__MODULE__, id)
    |> Map.get(:instructions)
  end

  def execute(scripts, %Mobile{} = mobile) do
    result =
      scripts
      |> Enum.find_value(fn(script) ->
           execute_script(script, mobile)
         end)
    result || mobile
  end

  def execute_script([instruction | rest], %Mobile{} = mobile) when is_list(instruction) do
    if mobile = execute_script(instruction, mobile) do
      mobile
    else
      execute_script(rest, mobile)
    end
  end

  def execute_script([instruction | script], %Mobile{} = mobile) do
    execute_instruction(instruction, mobile, script)
  end

  def execute_script([], mobile),   do: mobile
  def execute_script(nil, _mobile), do: false

  def execute_script(instruction, %Mobile{} = mobile), do: execute_instruction(instruction, mobile, nil)

  def execute_instruction(%{"message" => message}, %Mobile{} = mobile, script) do
    Mobile.send_scroll(mobile, "<p>#{message}</p>")
    execute_script(script, mobile)
  end

  def execute_instruction(%{"fail_flag" => %{"failure_message" => message, "flag" => flag}}, monster, script) do
    if Components.Flags.has_flag?(monster, flag) do
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
    else
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{"give_flag" => %{"flag" => flag, "value" => value}}, monster, script) do
    Components.Flags.set_flag(monster, flag, value)
    Entities.save!(monster)
    execute_script(script, monster)
  end

  def execute_instruction(%{"flag_equals" => %{"flag" => flag, "value" => value}}, monster, script) do
    if Components.Flags.flag_equals?(monster, flag, value) do
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{"flag_at_least" => %{"flag" => flag, "value" => value}}, monster, script) do
    if Components.Flags.flag_at_least?(monster, flag, value) do
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{"script_link" => new_script}, monster, script) do
    execute_script([new_script | script], monster)
  end

  def execute_instruction(%{"max_evil_points" => %{"failure_message" => message, "amount" => amount}}, monster, script) do
    if Components.Alignment.value(monster) <= amount do
      execute_script(script, monster)
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
    end
  end

  def execute_instruction(%{"check_item" => %{"failure_message" => message, "item" => item_template_id}}, %Mobile{} = mobile, script) do
    if Mobile.has_item?(mobile, item_template_id) do
      execute_script(script, mobile)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{message}</p>")
    end
  end

  def execute_instruction(%{"take_item" => %{"failure_message" => message, "item" => item_template_id}}, %Mobile{} = mobile, script) do
    if mobile = Mobile.remove_item?(mobile, item_template_id) do
      Spirit.save(mobile.spirit)
      execute_script(script, mobile)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{message}</p>")
    end
  end

  def execute_instruction(%{"add_delay" => delay}, %Mobile{delayed: false} = mobile, script) do
    Process.send_after(self, {:execute_script, script}, delay * 1000)
    execute_script([], Map.put(mobile, :delayed, true))
  end

  def execute_instruction(%{"add_delay" => _delay}, %Mobile{delayed: true} = mobile, _script) do
    execute_script([], mobile)
  end

  def execute_instruction(%{"give_item" => item_template_id}, %Mobile{} = mobile, script) do
    item = ApathyDrive.Item.generate_item(%{item_id: item_template_id, level: mobile.level})

    if Mobile.remaining_encumbrance(mobile) >= item["weight"] do
      mobile =
        put_in(mobile.spirit.inventory, [item | mobile.spirit.inventory])

      Repo.save!(mobile.spirit)
    else
      mobile.spirit.room_id
      |> Room.find
      |> Room.add_item(item)
    end

    execute_script(script, mobile)
  end

  def execute_instruction(%{"spawn_monster" => monster_template_id}, %Mobile{} = mobile, script) do
    room =
      mobile.room_id
      |> Room.find

    monster =
      monster_template_id
      |> MonsterTemplate.spawn_monster(Room.value(room))

    Task.start fn ->
      ApathyDrive.Exits.Normal.display_enter_message(room, monster)
    end

    execute_script(script, mobile)
  end

  def execute_instruction(%{"min_level" => %{"failure_message" => message, "level" => level}}, monster, script) do
    if Components.Level.value(monster) < level do
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
    else
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{"add_experience" => _exp}, %Monster{spirit: nil} = monster, script) do
    execute_script(script, monster)
  end

  def execute_instruction(%{"add_experience" => exp}, %Mobile{spirit: %Spirit{} = spirit} = mobile, script) do
    spirit = Spirit.add_experience(spirit, exp)
    execute_script(script, Map.put(mobile, :spirit, spirit))
  end

  def execute_instruction(%{"cast_ability" => ability_name}, monster, script) do
    case Abilities.find(ability_name) do
      nil ->
        Monster.send_scroll(monster, "<p><span class='red'>Not Implemented: #{ability_name}</span></p>")
      ability ->
        ability.execute(monster, nil)
    end
    execute_script(script, monster)
  end

  def execute_instruction(%{"random" => scripts}, mobile, script) do
    roll = :rand.uniform(100)

    number = scripts
            |> Map.keys
            |> Enum.map(&String.to_integer/1)
            |> Enum.sort
            |> Enum.find(&(&1 >= roll))

    if script do
      send(self, {:execute_script, script})
    end

    case scripts["#{number}"] do
      script_id when is_integer(script_id) ->
        script_id
        |> find
        |> execute_script(mobile)
      instructions ->
        execute_script(instructions, mobile)
    end
  end

  def execute_instruction(instruction, mobile, _script) do
    Mobile.send_scroll(mobile, "<p><span class='red'>Not Implemented: #{inspect instruction}</span></p>")
    false
  end

end
