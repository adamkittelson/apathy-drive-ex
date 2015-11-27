defmodule Commands.Unpossess do
  use ApathyDrive.Command

  def keywords, do: ["unpossess"]

  def execute(mobile, _arguments) do
    case Mobile.unpossess(mobile) do
      {:ok, spirit: spirit, mobile_name: mobile_name} ->
        Process.unlink(mobile)

        send(self, {:respawn, spirit: spirit})
        send(self, {:scroll, "<p>You leave the body of #{mobile_name}.</p>"})

      {:error, reason} ->
        Mobile.send_scroll(mobile, "<p>#{reason}</p>")
    end
  end

end
