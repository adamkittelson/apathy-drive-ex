defmodule ApathyDrive do

  require Weber.Templates.ViewsLoader

  def start(_type, _args) do

    :crypto.start
    :bcrypt.start
    :random.seed(:erlang.now)

    Players.start_link
    Races.start_link
    Characters.start_link
    Monsters.start_link
    MonsterTemplates.start_link
    Items.start_link
    ItemTemplates.start_link
    Rooms.start_link
    Exits.start_link
    Components.start_link
    Help.start_link
    Repo.start_link
    Commands.start_link
    Abilities.start_link
    Skills.start_link

    get_file_list(["game/**/abilities/**/*.ex", "game/**/commands/**/*.ex", "game/**/races/**/*.ex", "game/**/skills/**/*.ex"])
    |> Enum.each fn(file) ->
      IO.puts "Compiled #{file}"
      Code.load_file(file)
    end

    index_monsters
    index_abilities
    index_commands
    index_races
    index_skills

    # Set resources
    Weber.Templates.ViewsLoader.set_up_resources(File.cwd!)
    # compile all views
    Weber.Templates.ViewsLoader.compile_views(File.cwd!)

    if Mix.env != :test do
      IO.puts "Loading Entities..."
      Entities.load!
      IO.puts "Done!"
    end

    Systems.LairSpawning.initialize
    Systems.Regen.initialize
    Systems.Hints.initialize
    Systems.Idle.initialize

    # start weber application
    Weber.run_weber

  end

  def stop(_state) do
    :ok
  end

  defp get_file_list(path, file_index \\ []) when is_binary(path) do
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

  defp index_monsters do
    get_file_list(["lib/data/**/monsters/**/*.ex"])
    |> Enum.each(fn(file) ->
         module_name = Path.basename(file)
                       |> String.replace(".ex", "")
                       |> Inflex.camelize

        module = :"Elixir.Monsters.#{module_name}"

        {:ok, mt} = Entity.init
        Entity.add_component(mt, Components.Keywords, module.keywords)
        Entity.add_component(mt, Components.Name, module.name)
        Entity.add_component(mt, Components.Module, module)

        MonsterTemplates.add(file, mt)
      end)
  end

  defp index_abilities do
    get_file_list(["lib/data/**/abilities/**/*.ex"])
    |> Enum.each(fn(file) ->
         module_name = Path.basename(file)
                       |> String.replace(".ex", "")
                       |> Inflex.camelize

        module = :"Elixir.Abilities.#{module_name}"

        {:ok, ability} = Entity.init
        Entity.add_component(ability, Components.Keywords, module.keywords)
        Entity.add_component(ability, Components.Name, module.name)
        Entity.add_component(ability, Components.Module, module)
        Entity.add_component(ability, Components.Help, module.help)
        Abilities.add(module.name, ability)
        Help.add(ability)

      end)
  end

  defp index_commands do
    get_file_list(["lib/data/**/commands/**/*.ex"])
    |> Enum.each(fn(file) ->
         module_name = Path.basename(file)
                       |> String.replace(".ex", "")
                       |> Inflex.camelize
        module = :"Elixir.Commands.#{module_name}"

        {:ok, command} = Entity.init
        Entity.add_component(command, Components.Keywords, module.keywords)
        Entity.add_component(command, Components.Name, module.name)
        Commands.add(command)
      end)
  end

  defp index_races do
    get_file_list(["lib/data/**/races/**/*.ex"])
    |> Enum.each(fn(file) ->
         module_name = Path.basename(file)
                       |> String.replace(".ex", "")
                       |> Inflex.camelize
        module = :"Elixir.Races.#{module_name}"

        {:ok, race} = Entity.init
        Entity.add_component(race, Components.Keywords, module.keywords)
        Entity.add_component(race, Components.Name, module.name)
        Entity.add_component(race, Components.Help, module.help)
        Entity.add_component(race, Components.Limbs, module.limbs)
        Entity.add_component(race, Components.Stats, module.stats)
        Entity.add_component(race, Components.Module, module)
        Races.add(race)
        Help.add(race)
      end)
  end

  defp index_skills do
    get_file_list(["lib/data/**/skills/**/*.ex"])
    |> Enum.each(fn(file) ->
         module_name = Path.basename(file)
                       |> String.replace(".ex", "")
                       |> Inflex.camelize
        module = :"Elixir.Skills.#{module_name}"

        {:ok, skill} = Entity.init
        Entity.add_component(skill, Components.Keywords, module.keywords)
        Entity.add_component(skill, Components.Name, module.name)
        Entity.add_component(skill, Components.Module, module)
        Entity.add_component(skill, Components.Help, module.help)
        Skills.add(module.name, skill)
        Help.add(skill)
      end)
  end

end
