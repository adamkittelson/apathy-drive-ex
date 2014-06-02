defmodule Route do
  use Systems.Reload

  import Weber.Route
  require Weber.Route

  route on("GET", "/", :ApathyDrive.Main, :action)

end
