defmodule ApathyDrive.SpiritSupervisor do
  def start_link(options) do
    children = []

    Supervisor.start_link(children, options)
  end

end
