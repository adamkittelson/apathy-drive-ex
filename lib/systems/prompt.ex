defmodule Systems.Prompt do
  use Systems.Reload

  def update(entity) do
    Components.Player.send_message(entity, ["update prompt", "[HP=#{Components.HP.value(entity)}/MA=#{Components.Mana.value(entity)}]:"])
  end
end