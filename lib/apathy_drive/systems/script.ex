defmodule Systems.Script do
  import Utility
  use Systems.Reload

  def execute(scripts, monster) do
    scripts
    |> Enum.any?(fn(script) ->
         execute_script(script, monster)
       end)
  end

  def execute_script([instruction | _script], monster) when is_list(instruction) do
    execute(instruction, monster)
  end

  def execute_script([instruction | script], monster) do
    execute_instruction(instruction, monster, script)
  end

  def execute_script([], _monster), do: true

  def execute_script(instruction, monster), do: execute_instruction(instruction, monster, [])

  ########

  def execute_instruction(%{message: message}, monster, script) do
    send_message(monster, "scroll", "<p><span class='dark-green'>#{message}</p>")
    execute_script(script, monster)
  end

  def execute_instruction(instruction, monster, _script) do
    send_message(monster, "scroll", "<p><span class='red'>Not Implemented: #{inspect instruction}</span></p>")
    false
  end


end
