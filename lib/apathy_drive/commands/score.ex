defmodule Commands.Score do
  use ApathyDrive.Command

  def keywords, do: ["score", "stats", "status", "st"]

  def execute(mobile, _arguments) do

    score_data = Mobile.score_data(mobile)

    hp = String.ljust("#{trunc(score_data.hp)}/#{score_data.max_hp}", 13)
    mana = "#{trunc(score_data.mana)}/#{score_data.max_mana}"

    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>#{String.ljust(score_data.name, 13)}</span><span class='dark-green'>Class:</span> <span class='dark-cyan'>#{score_data.class}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>#{String.ljust(to_string(score_data.level), 12)}</span><span class='dark-green'>Exp:</span> <span class='dark-cyan'>#{score_data.experience}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Hits:</span> <span class='dark-cyan'>#{hp}</span><span class='dark-green'>Mana:</span> <span class='dark-cyan'>#{mana}</span></p>")
    Mobile.send_scroll(mobile, "\n\n")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Physical Defense:</span> <span class='dark-cyan'>#{score_data.physical_defense}</span> <span class='dark-green'>Magical Defense:</span> <span class='dark-cyan'>#{score_data.magical_defense}</span></p>")
    Mobile.send_scroll(mobile, "\n\n")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Strength:</span> <span class='dark-cyan'>#{score_data.strength}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Agility:</span>  <span class='dark-cyan'>#{score_data.agility}</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Will:</span>     <span class='dark-cyan'>#{score_data.will}</span></p>")

    Enum.each(score_data.effects, fn(effect_message) ->
      Mobile.send_scroll(mobile, "<p>#{effect_message}</p>")
    end)

  end

end
