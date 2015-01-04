defmodule Systems.Light do
  use Systems.Reload

  def light(item) do
    template = Components.Module.value(item)
    cond do
      !template.light ->
        :not_a_light
      lit?(item) ->
        :already_lit
      !!template.light_duration ->
        {:ok, tm} = TimerManager.start
        Entity.add_component(item, Components.TimerManager, tm)

        Components.TimerManager.call_every(item, {:light, 1000, fn ->
          Components.Uses.use!(item)
        end})
        Components.Effects.add(item, :light, %{light: template.light, timers: [:light]})
      true ->
        Components.Effects.add(item, :light, %{light: template.light})
    end
  end

  def extinguish(item) do
    template = Components.Module.value(item)
    cond do
      !template.light ->
        :not_a_light
      !lit?(item) ->
        :not_lit
      template.always_lit ->
        :always_lit
      true ->
        Components.Effects.remove(item, :light)
    end
  end

  def lit?(item) do
    item
    |> Components.Effects.value
    |> Map.values
    |> Enum.any?(fn(effect) ->
         Map.has_key?(effect, :light)
       end)
  end
end