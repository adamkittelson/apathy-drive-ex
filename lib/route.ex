defmodule Route do

  import Weber.Route
  require Weber.Route

  route on("GET", "/", :ApathyDrive.Main, :action)

end
