defmodule Systems.Prompt do
  use Systems.Reload
  import Utility

  def display(%Spirit{} = spirit) do
    spirit
    |> Spirit.send_disable("#prompt")
    |> Spirit.send_disable("#command")
    |> Spirit.send_scroll("<p><span id='prompt'>#{prompt(spirit)}</span><input id='command' size='50' class='prompt'></input></p>")
    |> Spirit.send_focus("#command")
    |> Spirit.send_up
  end

  def display(%Spirit{} = spirit, %Monster{} = monster) do
    spirit
    |> Spirit.send_disable("#prompt")
    |> Spirit.send_disable("#command")
    |> Spirit.send_scroll("<p><span id='prompt'>#{prompt(spirit, monster)}</span><input id='command' size='50' class='prompt'></input></p>")
    |> Spirit.send_focus("#command")
    |> Spirit.send_up
  end

  def update(nil, _), do: nil

  def update(%Spirit{} = spirit) do
    spirit
    |> Spirit.send_update_prompt(prompt(spirit))
  end

  def update(%Spirit{} = spirit, %Monster{} = monster) do
    Spirit.send_update_prompt(spirit, prompt(spirit, monster))
  end

  def prompt(%Spirit{} = spirit) do
    "[Level=#{spirit.level}/Exp=#{spirit.experience}]:"
  end

  def prompt(%Spirit{} = spirit, %Monster{} = monster) do
    "[HP=#{monster.hp}/MA=#{monster.mana}]:"
  end
end
