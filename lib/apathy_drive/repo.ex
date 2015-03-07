defmodule ApathyDrive.Repo do

  use Ecto.Repo, otp_app: :apathy_drive,
                 adapter: Ecto.Adapters.Postgres

end
