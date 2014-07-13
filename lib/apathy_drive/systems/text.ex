defmodule Systems.Text do
  use Systems.Reload

  def interpolate(string, opts) do

    if opts["user"] do
      user = opts["user"]
      if Entity.has_component?(user, Components.Gender) do
        string = case Components.Gender.value(user) do
                   "male"   ->
                     String.replace(string, ~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/, "\\1")
                   "female" ->
                     String.replace(string, ~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/, "\\2")
                   other ->
                     String.replace(string, ~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/, "\\3")
                 end
      else
        string = String.replace(string, ~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/, "\\3")
      end
    end


    if opts["target"] do
      target = opts["target"]
      string = String.replace(string, ~r/\{\{target\}\}/, Components.Name.value(target))
      if Entity.has_component?(target, Components.Gender) do
        string = case Components.Gender.value(target) do
                   "male"   ->
                     String.replace(string, ~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/, "\\1")
                   "female" ->
                     String.replace(string, ~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/, "\\2")
                   other ->
                     String.replace(string, ~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/, "\\3")
                 end
      else
        string = String.replace(string, ~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/, "\\3")
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
         String.replace(updated_string, ~r/\{\{#{interpolation}\}\}/, "#{value}")
       end)
  end

  def capitalize_first(string) do
    {first, rest} = String.split_at(string, 1)
    name = "#{String.capitalize(first)}#{rest}"
  end

  def strip_tags(string), do: String.replace(string, ~r/<[^>]*>/, "")

end
