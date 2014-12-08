defmodule Systems.Prompt do
  use Systems.Reload
  import Utility

  def display(nil, _), do: nil

  def display(spirit, nil) do
    send_message(spirit, "disable", "#prompt")
    send_message(spirit, "disable", "#command")
    send_message(spirit, "scroll", "<p><span id='prompt'>#{prompt(spirit, nil)}</span><input id='command' size='50' class='prompt'></input></p>")
    send_message(spirit, "focus", "#command")
    send_message(spirit, "up")
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

  def prompt(spirit, nil) do
    "[Level=#{Components.Level.value(spirit)}/Exp=#{Components.Experience.value(spirit)}]:"
  end

  def prompt(spirit, monster) do
    "[HP=#{Components.HP.value(monster)}/MA=#{Components.Mana.value(monster)}]:"
  end
end
