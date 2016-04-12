defmodule ApathyDrive.Commands.Say do
  use ApathyDrive.Command

  def keywords, do: ["say"]

  def execute(mobile, arguments) when is_pid(mobile) do
    Mobile.say(mobile, Enum.join(arguments, " "))
  end

  def execute(%Mobile{} = mobile, message) do
    message = Mobile.sanitize(message)
    ApathyDrive.PubSub.broadcast_from! self(), "rooms:#{mobile.room_id}:mobiles", {:say, %{name: mobile.name, unity: mobile.spirit && mobile.spirit.unity || mobile.unity}, message}
    Mobile.send_scroll(mobile, "<p>You say: <span class='dark-green'>\"#{message}\"</span></p>")
  end

end
