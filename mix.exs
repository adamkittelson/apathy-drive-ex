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
      { :weber,    github: "adamkittelson/weber", ref: "d3d0e0f9ce497ff5699f45226636e77613670628" },
      { :ecto,     "~> 0.2.0" },
      { :jazz,     github: "meh/jazz", ref: "7af3b74e58eb1a3fc6b9874a2077efa420f6dfcc" },
      { :postgrex, "~> 0.5.1" },
      { :bcrypt,   github: "Feuerlabs/erlang-bcrypt"},
      { :inflex,   github: "nurugger07/inflex", ref: "2b3e3267122f7ad6276263cdaca18157d0cee2f7"}
    ]
  end

  defp deps(:test) do
    deps(:prod)
  end

  defp deps(_) do
    deps(:prod)
  end
end
