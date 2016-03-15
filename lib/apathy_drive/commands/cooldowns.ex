defmodule ApathyDrive.Commands.Cooldowns do
  use ApathyDrive.Command

  def keywords, do: ["cooldowns", "cd"]

  def execute(mobile, _arguments) do
    Mobile.send_scroll(mobile, "<p><span class='white'>The following abilities are on cooldown:</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-magenta'>Ability Name    Remaining</span></p>")
    Mobile.display_cooldowns(mobile)
  end

end
