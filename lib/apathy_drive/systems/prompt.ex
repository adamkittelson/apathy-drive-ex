defmodule Systems.Prompt do
  use Systems.Reload
  import Utility

  def update(nil, _), do: nil

  def update(spirit, nil) do
    send_message(spirit, "update prompt", "[#{Systems.Trainer.total_power(spirit)}]:")
  end

  def update(spirit, monster) do
    send_message(spirit, "update prompt", "[HP=#{Components.HP.value(monster)}/MA=#{Components.Mana.value(monster)}]:")
  end
end