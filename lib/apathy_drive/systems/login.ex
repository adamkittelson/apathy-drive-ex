defmodule Systems.Login do
  use Systems.Reload
  import Utility

  def create do
    url = Systems.URL.random

    Spirit.create(url)

    url
  end

  def login(socket, url) do
    spirit = Spirit.find_by_url(url)
    if spirit do
      spirit = Map.put(spirit, :socket, socket)

      spirit = Spirit.login(spirit)

      Spirit.activate_hint(spirit, "movement")
      Spirit.activate_hint(spirit, "name")
      Possession.unpossess(spirit)
      spirit
    end
  end

end
