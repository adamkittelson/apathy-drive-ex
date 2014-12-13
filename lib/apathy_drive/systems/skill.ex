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
        modified = if base > 0 do
          total = Map.keys(modifiers) |> Enum.reduce(0, fn(stat, total) ->
                                           total + Systems.Stat.modified(entity, "#{stat}") * modifiers[stat]
                                         end)

          average = total / (Map.values(modifiers) |> Enum.sum)

          round(base * (1 + average * 0.02))
        else
          0
        end
        modified + effects_bonus(entity)
      end

      def effects_bonus(entity) do
        entity
        |> Components.Effects.value
        |> Map.values
        |> Enum.map(fn
             (%{} = effect) ->
               effect[__MODULE__.name] || effect[__MODULE__.name |> String.to_atom] || 0
             (_) ->
               0
           end)
        |> Enum.sum
      end

      def universal?, do: true

      defoverridable [universal?: 0]

    end
  end

end
