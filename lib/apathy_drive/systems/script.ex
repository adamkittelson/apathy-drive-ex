defmodule Systems.Script do

  def execute(scripts, monster) do
    scripts
    |> Enum.any?(fn(script) ->
         execute_script(script, monster)
       end)
    monster
  end

  def execute_script([instruction | rest], monster) when is_list(instruction) do
    unless execute_script(instruction, monster) do
      execute_script(rest, monster)
    end
  end

  def execute_script([instruction | script], monster) do
    execute_instruction(instruction, monster, script)
  end

  def execute_script([], _monster),  do: true
  def execute_script(nil, _monster), do: false

  def execute_script(instruction, monster), do: execute_instruction(instruction, monster, nil)

  def execute_instruction(%{"message" => message}, monster, script) do
    Monster.send_scroll(monster, "<p>#{message}</p>")
    execute_script(script, monster)
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

  def execute_instruction(%{"give_item" => item}, monster, script) do
    it = ItemTemplate.find(item)
    if it do
      ItemTemplate.spawn_item(it, monster)
    end
    execute_script(script, monster)
  end

  def execute_instruction(%{"fail_item" => %{"failure_message" => message, "item" => item}}, monster, script) do
    if Systems.Item.has_item?(monster, item) do
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
    else
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{"check_item" => %{"failure_message" => message, "item" => item}}, monster, script) do
    if Systems.Item.has_item?(monster, item) do
      execute_script(script, monster)
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
    end
  end

  def execute_instruction(%{"take_item" => %{"failure_message" => message, "item" => item}}, monster, script) do
    i = Components.Items.get_items(monster)
        |> Enum.find(fn(i) ->
             Components.Name.value(i) == item
           end)
    if i do
      Components.Items.remove_item(monster, i)
      Entities.delete!(i)
      execute_script(script, monster)
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{message}</p>")
    end
  end

  def execute_instruction(%{"add_experience" => experience}, monster, script) do
    old_power = Systems.Trainer.total_power(monster)
    Components.Experience.add(monster, experience)
    new_power = Systems.Trainer.total_power(monster)
    power_gain = new_power - old_power
    if power_gain > 0 do
      Monster.send_scroll(monster, "<p>Your #{Components.Name.value(monster)} gains #{power_gain} power.</p>")
    end
    execute_script(script, monster)
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
    :random.seed(:os.timestamp)
    roll = :random.uniform(100)

    number = scripts
            |> Map.keys
            |> Enum.sort
            |> Enum.find(&(&1 >= roll))

    execute_script(scripts[number], monster)

    execute_script(script, monster)
  end

  def execute_instruction(instruction, monster, _script) do
    Monster.send_scroll(monster, "<p><span class='red'>Not Implemented: #{inspect instruction}</span></p>")
    false
  end

end
