defmodule Route do
  use Systems.Reload

  import Weber.Route
  require Weber.Route

  route on("GET", "/",         :ApathyDrive.Main, :home)
     |> on("GET", "/game/:id", :ApathyDrive.Main, :game)
     |> on("POST", "/game",    :ApathyDrive.Main, :game)

end
