defmodule Systems.Skill do
  use Systems.Reload

  def base(entity) do
    Components.Skills.list(entity)
    |> Enum.reduce(%{}, fn(skill, skills) ->
         Map.put(skills, skill, base(entity, skill))
       end)
  end

  def base(entity, skill) do
    Skills.find(skill).base(entity)
  end

  def modified(entity) do
    Components.Skills.list(entity)
    |> Enum.reduce(%{}, fn(skill, skills) ->
         Map.put(skills, skill, modified(entity, skill))
       end)
  end

  def modified(entity, skill) do
    Skills.find(skill).modified(entity)
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      @after_compile Systems.Skill

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
        |> String.replace("_", " ")
      end

      def keywords do
        name |> String.split
      end

      def base(entity) do
         trained(entity) + Components.Skills.base(entity, name)
      end

      def trained(entity) do
        Systems.Trainer.rating(__MODULE__, entity)
      end

      def modified(entity) do
        base = base(entity)
        if base > 0 do
          total = Map.keys(modifiers) |> Enum.reduce(0, fn(stat, total) ->
                                           total + Systems.Stat.modified(entity, "#{stat}") * modifiers[stat]
                                         end)

          average = total / (Map.values(modifiers) |> Enum.sum)

          round(base * (1 + (average - 40) * 0.02))
        else
          0
        end
      end

    end
  end

  defmacro __after_compile__(_env, _bytecode) do
    quote do
      {:ok, skill} = Entity.init
      Entity.add_component(skill, Components.Keywords, __MODULE__.keywords)
      Entity.add_component(skill, Components.Name, __MODULE__.name)
      Entity.add_component(skill, Components.Module, __MODULE__)
      Entity.add_component(skill, Components.Help, __MODULE__.help)
      Skills.add(__MODULE__.name, skill)
      Help.add(skill)
    end
  end

end
