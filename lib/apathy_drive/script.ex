defmodule ApathyDrive.Script do
  alias ApathyDrive.Mobile

  def execute(scripts, %Mobile{} = mobile) do
    scripts
    |> Enum.any?(fn(script) ->
         execute_script(script, mobile)
       end)
    mobile
  end

  def execute_script([instruction | rest], %Mobile{} = mobile) when is_list(instruction) do
    unless execute_script(instruction, mobile) do
      execute_script(rest, mobile)
    end
  end

  def execute_script([instruction | script], %Mobile{} = mobile) do
    execute_instruction(instruction, mobile, script)
  end

  def execute_script([], _mobile),  do: true
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

  def execute_instruction(%{"random" => scripts}, monster, script) do
    roll = :random.uniform(100)

    number = scripts
            |> Map.keys
            |> Enum.map(&String.to_integer/1)
            |> Enum.sort
            |> Enum.find(&(&1 >= roll))

    execute_script(scripts["#{number}"], monster)

    execute_script(script, monster)
  end

  def execute_instruction(instruction, mobile, _script) do
    Mobile.send_scroll(mobile, "<p><span class='red'>Not Implemented: #{inspect instruction}</span></p>")
    false
  end

end
