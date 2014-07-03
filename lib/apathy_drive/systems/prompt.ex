defmodule Systems.Prompt do
  use Systems.Reload
  import Utility

  def update(entity) do
    send_message(entity, "update prompt", "[HP=#{Components.HP.value(entity)}/MA=#{Components.Mana.value(entity)}]:")
  end
end