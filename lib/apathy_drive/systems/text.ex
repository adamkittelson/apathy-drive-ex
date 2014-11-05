defmodule Systems.Text do
  use Systems.Reload

  def interpolate(string, opts) do

    if opts["user"] do
      user = opts["user"]
      if Entity.has_component?(user, Components.Gender) do
        string = case Components.Gender.value(user) do
                   "male"   ->
                     string = Regex.replace(~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/,
                                            string, fn(_, m, _, _) -> m end)
                     string = Regex.replace(~r/\{\{User:(.+?)\/(.+?)\/(.+?)\}\}/,
                                            string, fn(_, m, _, _) -> capitalize_first(m) end)
                   "female" ->
                     string = Regex.replace(~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/,
                                             string, fn(_, _, f, _) -> f end)
                     string = Regex.replace(~r/\{\{User:(.+?)\/(.+?)\/(.+?)\}\}/,
                                             string, fn(_, _, f, _) -> capitalize_first(f) end)
                   _other ->
                     string = Regex.replace(~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/,
                                             string, fn(_, _, _, o) -> o end)
                     string = Regex.replace(~r/\{\{User:(.+?)\/(.+?)\/(.+?)\}\}/,
                                             string, fn(_, _, _, o) -> capitalize_first(o) end)
                 end
      else
        string = Regex.replace(~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/,
                                string, fn(_, _, _, o) -> o end)
        string = Regex.replace(~r/\{\{User:(.+?)\/(.+?)\/(.+?)\}\}/,
                                string, fn(_, _, _, o) -> capitalize_first(o) end)
      end
    end


    if opts["target"] do
      target = opts["target"]
      if Entity.has_component?(target, Components.Gender) do
        string = case Components.Gender.value(target) do
                   "male"   ->
                     string = Regex.replace(~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/,
                                            string, fn(_, m, _, _) -> m end)
                     string = Regex.replace(~r/\{\{Target:(.+?)\/(.+?)\/(.+?)\}\}/,
                                            string, fn(_, m, _, _) -> capitalize_first(m) end)
                   "female" ->
                     string = Regex.replace(~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/,
                                             string, fn(_, _, f, _) -> f end)
                     string = Regex.replace(~r/\{\{Target:(.+?)\/(.+?)\/(.+?)\}\}/,
                                             string, fn(_, _, f, _) -> capitalize_first(f) end)
                   _other ->
                     string = Regex.replace(~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/,
                                             string, fn(_, _, _, o) -> o end)
                     string = Regex.replace(~r/\{\{Target:(.+?)\/(.+?)\/(.+?)\}\}/,
                                             string, fn(_, _, _, o) -> capitalize_first(o) end)
                 end
      else
        string = Regex.replace(~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/,
                                string, fn(_, _, _, o) -> o end)
        string = Regex.replace(~r/\{\{Target:(.+?)\/(.+?)\/(.+?)\}\}/,
                                string, fn(_, _, _, o) -> capitalize_first(o) end)
      end
      
    end

    opts
    |> Map.keys
    |> Enum.reduce(string, fn(interpolation, updated_string) ->
         value = if is_pid opts[interpolation] do
           Components.Name.value(opts[interpolation])
         else
           opts[interpolation]
         end
         updated_string
         |> String.replace(~r/\{\{#{capitalize_first(interpolation)}\}\}/, "#{capitalize_first(to_string(value))}")
         |> String.replace(~r/\{\{#{interpolation}\}\}/, "#{value}")
       end)
  end

  def capitalize_first(string) do
    {first, rest} = String.split_at(string, 1)
    "#{String.capitalize(first)}#{rest}"
  end

  def strip_tags(string), do: String.replace(string, ~r/<[^>]*>/, "")

end
