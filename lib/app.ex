defmodule ApathyDrive do

  require Weber.Templates.ViewsLoader

  def start(_type, _args) do
    Players.start_link
    Repo.start_link
    # Set resources
    Weber.Templates.ViewsLoader.set_up_resources(File.cwd!)
    # compile all views
    Weber.Templates.ViewsLoader.compile_views(File.cwd!)

    IO.puts "Loading Entities..."
    ApathyDrive.Entity.load!
    IO.puts "Done!"

    # start weber application
    Weber.run_weber

  end

  def stop(_state) do
    :ok
  end

end
