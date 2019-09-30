defmodule ApathyDrive.Text do
  def interpolate(nil, _opts), do: nil

  def interpolate(string, opts) do
    string =
      if opts["user"] do
        user = opts["user"]

        if Map.has_key?(user, :gender) do
          case user.gender do
            "male" ->
              string =
                Regex.replace(~r/\{\{user:(.+?)\/(.+?)\/(.+?)?\}\}/, string, fn _, m, _, _ ->
                  m
                end)

              Regex.replace(~r/\{\{User:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, m, _, _ ->
                capitalize_first(m)
              end)

            "female" ->
              string =
                Regex.replace(~r/\{\{user:(.+?)\/(.+?)\/(.+?)?\}\}/, string, fn _, _, f, _ ->
                  f
                end)

              Regex.replace(~r/\{\{User:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, f, _ ->
                capitalize_first(f)
              end)

            _other ->
              string =
                Regex.replace(~r/\{\{user:(.+?)\/(.+?)\/(.+?)?\}\}/, string, fn _, _, _, o ->
                  o
                end)

              Regex.replace(~r/\{\{User:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, _, o ->
                capitalize_first(o)
              end)
          end
        else
          string =
            Regex.replace(~r/\{\{user:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, _, o -> o end)

          Regex.replace(~r/\{\{User:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, _, o ->
            capitalize_first(o)
          end)
        end
      else
        string
      end

    string =
      if opts["target"] do
        target = opts["target"]

        if Map.has_key?(target, :gender) do
          case target.gender do
            "male" ->
              string =
                Regex.replace(~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, m, _, _ ->
                  m
                end)

              Regex.replace(~r/\{\{Target:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, m, _, _ ->
                capitalize_first(m)
              end)

            "female" ->
              string =
                Regex.replace(~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, f, _ ->
                  f
                end)

              Regex.replace(~r/\{\{Target:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, f, _ ->
                capitalize_first(f)
              end)

            _other ->
              string =
                Regex.replace(~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, _, o ->
                  o
                end)

              Regex.replace(~r/\{\{Target:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, _, o ->
                capitalize_first(o)
              end)
          end
        else
          string =
            Regex.replace(~r/\{\{target:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, _, o -> o end)

          Regex.replace(~r/\{\{Target:(.+?)\/(.+?)\/(.+?)\}\}/, string, fn _, _, _, o ->
            capitalize_first(o)
          end)
        end
      else
        string
      end

    opts
    |> Map.keys()
    |> Enum.reduce(string, fn interpolation, updated_string ->
      value =
        if is_map(opts[interpolation]) do
          opts[interpolation].name
        else
          opts[interpolation]
        end

      updated_string
      |> String.replace(
        ~r/\{\{#{capitalize_first(interpolation)}\}\}/,
        "#{capitalize_first(to_string(value))}"
      )
      |> String.replace(~r/\{\{#{interpolation}\}\}/, "#{value}")
    end)
  end

  def capitalize_first(string) do
    {first, rest} = String.split_at(string, 1)
    "#{String.capitalize(first)}#{rest}"
  end

  def strip_tags(string), do: String.replace(string, ~r/<[^>]*>/, "")
end
