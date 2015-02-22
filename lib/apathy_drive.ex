defmodule ApathyDrive do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Monsters.start_link
    MonsterTemplates.start_link
    Items.start_link
    ItemTemplates.start_link
    Exits.start_link
    Components.start_link
    Help.start_link
    Abilities.start_link
    Skills.start_link
    Corpses.start_link
    Parent.start_link
    CritTables.start_link
    Possession.start_link

    children = [
      worker(ApathyDrive.Repo, []),
      worker(ApathyDrive.Ticks, [%{timers: %{}}]),
      worker(ApathyDrive.Endpoint, [])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: ApathyDrive.Supervisor]
    supervisor = Supervisor.start_link(children, opts)

    if Mix.env != :test do
      IO.puts "Indexing..."
      [index_help,
      index_crit_tables]
      |> Enum.each(&(Task.await(&1, 30000)))
      IO.puts "Done!"
    end

    set_parents

    Task.async fn ->
      Monsters.all |> Enum.each(&Components.Abilities.reset_abilities/1)
    end

    Systems.Spawning.initialize
    Systems.Decay.initialize
    Systems.RoomAbility.initialize
    supervisor
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    ApathyDrive.Endpoint.config_change(changed, removed)
    :ok
  end

  defp set_parents do
    ["Items", "Monsters"]
    |> Enum.each(fn(type) ->
         :"Elixir.Components.#{type}"
         |> Components.all
         |> Enum.each(fn(parent) ->
              parent
              |> :"Elixir.Components.#{type}".value
              |> Enum.each(fn(item_id) ->
                   :"Elixir.#{type}".find_by_id(item_id)
                   |> Parent.set(parent)
                 end)
            end)
       end)
  end

  defp get_file_list(path, file_index \\ [])

  defp get_file_list(path, file_index) when is_binary(path) do
    get_file_list([path], file_index)
  end

  defp get_file_list([], file_index) do
    file_index
  end

  defp get_file_list([path | paths], file_index) do
    updated_file_index = Enum.reduce Path.wildcard(path), file_index, fn(file_path, index)->
      {:ok, file_info} = File.stat(file_path)
      if file_info.type == :directory || Enum.member?(index, file_path) do
        index
      else
        [file_path | index]
      end
    end

    get_file_list(paths, updated_file_index)
  end

  defp index_help do
    Task.async fn ->
    get_file_list(["lib/apathy_drive/data/**/help/**/*.ex"])
    |> Enum.each(fn(file) ->
         module_name = Path.basename(file)
                       |> String.replace(".ex", "")
                       |> Inflex.camelize
        module = :"Elixir.Help.#{module_name}"

        {:ok, help} = Entity.init
        Entity.add_component(help, Components.Keywords, module.keywords)
        Entity.add_component(help, Components.Name, module.name)
        Entity.add_component(help, Components.Module, module)
        Entity.add_component(help, Components.Help, module.help)
        Help.add(help)
      end)
    end
  end

  defp index_crit_tables do
    Task.async fn ->
    get_file_list(["lib/apathy_drive/data/**/crit_tables/**/*.ex"])
    |> Enum.each(fn(file) ->
         module_name = Path.basename(file)
                       |> String.replace(".ex", "")
                       |> Inflex.camelize
        module = :"Elixir.CritTables.#{module_name}"

        CritTables.add(module.name, module)

      end)
    end
  end
end
