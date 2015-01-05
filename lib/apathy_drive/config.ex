defmodule ApathyDrive.Config do

  def get(key) do
    Application.get_all_env(:apathy_drive)[ApathyDrive.Endpoint][key]
  end

end
