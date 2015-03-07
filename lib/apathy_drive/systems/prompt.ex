defmodule Systems.Prompt do



  def display(%Spirit{} = spirit) do
    spirit
    |> Spirit.send_disable("#prompt")
    |> Spirit.send_disable("#command")
    |> Spirit.send_scroll("<p><span id='prompt'>#{prompt(spirit)}</span><input id='command' size='50' class='prompt'></input></p>")
    |> Spirit.send_focus("#command")
    |> Spirit.send_up
  end

  def display(%Monster{} = monster) do
    monster
    |> Monster.send_disable("#prompt")
    |> Monster.send_disable("#command")
    |> Monster.send_scroll("<p><span id='prompt'>#{prompt(monster)}</span><input id='command' size='50' class='prompt'></input></p>")
    |> Monster.send_focus("#command")
    |> Monster.send_up
  end

  def update(%Spirit{} = spirit) do
    spirit
    |> Spirit.send_update_prompt(prompt(spirit))
  end

  def update(%Monster{} = monster) do
    Monster.send_update_prompt(monster, prompt(monster))
  end

  def prompt(%Spirit{} = spirit) do
    "[Level=#{spirit.level}/Exp=#{spirit.experience}]:"
  end

  def prompt(%Monster{} = monster) do
    "[HP=#{monster.hp}/MA=#{monster.mana}]:"
  end
end
