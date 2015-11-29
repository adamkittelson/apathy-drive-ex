defmodule Commands.Forms do
  use ApathyDrive.Command

  def keywords, do: ["forms", "recipes"]

  def execute(mobile, args) do
    Mobile.list_forms(mobile, Enum.join(args, " "))
  end
end
