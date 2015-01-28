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

  def display(spirit, monster) do
    send_message(spirit, "disable", "#prompt")
    send_message(spirit, "disable", "#command")
    send_message(spirit, "scroll", "<p><span id='prompt'>#{prompt(spirit, monster)}</span><input id='command' size='50' class='prompt'></input></p>")
    send_message(spirit, "focus", "#command")
    send_message(spirit, "up")
  end

  def update(nil, _), do: nil

  def update(spirit, nil) do
    send_message(spirit, "update prompt", prompt(spirit, nil))
  end

  def update(spirit, monster) do
    send_message(spirit, "update prompt", prompt(spirit, monster))
  end

  def prompt(%Spirit{} = spirit) do
    "[Level=#{spirit.level}/Exp=#{spirit.experience}]:"
  end

  def prompt(spirit, monster) do
    "[HP=#{Components.HP.value(monster)}/MA=#{Components.Mana.value(monster)}]:"
  end
end
