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
      Mobile.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
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

  def execute_instruction(%{"allowed_alignments" => %{"failure_message" => message, "alignments" => alignments}}, %Mobile{alignment: alignment} = mobile, script) do
    if alignment in alignments do
      execute_script(script, mobile)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{message}</p>")
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

  def execute_instruction(%{"check_hitpoints" => %{"modifier" => amount, "failure_script" => failure_script}}, %Mobile{hp: hp} = mobile, script) do
    if hp < :rand.uniform(100 + amount) do
      execute_script(find(failure_script), mobile)
    else
      execute_script(script, mobile)
    end
  end

  def execute_instruction(%{"min_evil_points" => %{"amount" => amount, "failure_message" => failure_message}}, %Mobile{} = mobile, script) do
    if Mobile.evil_points(mobile) < amount do
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{failure_message}</p>")
    else
      execute_script(script, mobile)
    end
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
    mobile.room_id
    |> Room.find
    |> Room.create_monster(monster_template_id)

    execute_script(script, mobile)
  end

  def execute_instruction(%{"min_level" => %{"failure_message" => message, "level" => level}}, monster, script) do
    if Components.Level.value(monster) < level do
      Mobile.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
    else
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{"add_experience" => exp}, %Mobile{spirit: %Spirit{} = spirit} = mobile, script) do
    spirit = Spirit.add_experience(spirit, exp)
    execute_script(script, Map.put(mobile, :spirit, spirit))
  end

  def execute_instruction(%{"cast_ability" => ability_id}, %Mobile{} = mobile, script) do
    case ApathyDrive.Ability.find(ability_id) do
      nil ->
        Mobile.send_scroll(mobile, "<p><span class='red'>Not Implemented: Ability ##{ability_id}</span></p>")
      ability ->
        ability = Map.put(ability, "ignores_global_cooldown", true)
        send(self, {:execute_ability, ability})
    end
    execute_script(script, mobile)
  end

  def execute_instruction(%{"room_item" => %{"failure_message" => failure_message, "item" => item_name}}, %Mobile{room_id: room_id} = mobile, script) do
    room = Room.find(room_id)

    if Room.find_item(room, item_name) do
      execute_script(script, mobile)
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{failure_message}</p>")
    end
  end

  def execute_instruction(%{"price" => %{"failure_message" => failure_message, "price_in_copper" => _price}}, %Mobile{spirit: nil} = mobile, _script) do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{failure_message}</p>")
  end

  def execute_instruction(%{"price" => %{"failure_message" => failure_message, "price_in_copper" => price}}, %Mobile{spirit: %Spirit{experience: exp} = spirit} = mobile, script) do
    if exp >= price do
      spirit = Spirit.add_experience(spirit, -price)
      execute_script(script, Map.put(mobile, :spirit, spirit))
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{failure_message}</p>")
    end
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
    execute_script([], mobile)
  end

end
