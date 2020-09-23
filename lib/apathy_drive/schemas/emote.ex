defmodule ApathyDrive.Emote do
  use ApathyDriveWeb, :model
  alias ApathyDrive.Emote

  schema "emotes" do
    field(:command, :string)
    field(:user_message, :string)
    field(:target_message, :string)
    field(:spectator_message, :string)
    field(:targeted, :boolean)
  end
end
