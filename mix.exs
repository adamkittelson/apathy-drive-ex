defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [
      app: :apathy_drive,
      version: "0.0.1",
      deps: deps(Mix.env)
    ]
  end

  def application do
    [
      applications: [],
      mod: {ApathyDrive, []}
    ]
  end

  defp deps(:prod) do
    [
      { :weber,    github: "elixir-web/weber", ref: "bd79f3a3cf3c7be4586b9fe2e0251432202d5432" },
      { :ecto,     "~> 0.2.0" },
      { :jazz,     github: "meh/jazz", ref: "7af3b74e58eb1a3fc6b9874a2077efa420f6dfcc" },
      { :postgrex, "~> 0.5.1" },
      { :bcrypt,   github: "Feuerlabs/erlang-bcrypt"},
      { :inflex,   "~> 0.2.1" }
    ]
  end

  defp deps(:test) do
    deps(:prod)
  end

  defp deps(_) do
    deps(:prod)
  end
end
