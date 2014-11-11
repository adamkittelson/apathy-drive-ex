defmodule Systems.Script do
  import Utility
  use Systems.Reload

  def execute(scripts, monster) do
    scripts
    |> Enum.any?(fn(script) ->
         execute_script(script, monster)
       end)
  end

  def execute_script([instruction | rest], monster) when is_list(instruction) do
    unless execute_script(instruction, monster) do
      execute_script(rest, monster)
    end
  end

  def execute_script([instruction | script], monster) do
    execute_instruction(instruction, monster, script)
  end

  def execute_script([], _monster), do: true

  def execute_script(instruction, monster), do: execute_instruction(instruction, monster, [])

  def execute_instruction(%{message: message}, monster, script) do
    send_message(monster, "scroll", "<p>#{message}</p>")
    execute_script(script, monster)
  end

  def execute_instruction(%{fail_flag: %{failure_message: message, flag: flag}}, monster, script) do
    if Components.Flags.has_flag?(monster, flag) do
      send_message(monster, "scroll", "<p><span class='dark-green'>#{message}</p>")
    else
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{give_flag: %{flag: flag, value: value}}, monster, script) do
    Components.Flags.set_flag(monster, flag, value)
    Entities.save!(monster)
    execute_script(script, monster)
  end

  def execute_instruction(%{flag_equals: %{flag: flag, value: value}}, monster, script) do
    if Components.Flags.flag_equals?(monster, flag, value) do
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{flag_at_least: %{flag: flag, value: value}}, monster, script) do
    if Components.Flags.flag_at_least?(monster, flag, value) do
      execute_script(script, monster)
    end
  end

  def execute_instruction(%{script_link: new_script}, monster, script) do
    execute_script([new_script | script], monster)
  end

  def execute_instruction(%{max_evil_points: %{failure_message: message, amount: amount}}, monster, script) do
    if Components.Alignment.value(monster) <= amount do
      execute_script(script, monster)
    else
      send_message(monster, "scroll", "<p><span class='dark-green'>#{message}</p>")
    end
  end

  def execute_instruction(instruction, monster, _script) do
    send_message(monster, "scroll", "<p><span class='red'>Not Implemented: #{inspect instruction}</span></p>")
    false
  end

end
