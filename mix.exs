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
      { :weber, github: "adamkittelson/weber", ref: "a8dbe3379b5457db5b8839aa311bda913775f82e" },
      {:decimal, github: "ericmj/decimal", tag: "v0.1.1", override: true},
      {:postgrex, github: "ericmj/postgrex", ref: "ec804cbc9e84ef75c04c0e1ba52c7f4e8d9bfe56", override: true},
      {:ecto, github: "elixir-lang/ecto" },
      { :bcrypt, github: "Feuerlabs/erlang-bcrypt"}
    ]
  end

  defp deps(:test) do
    deps(:prod) ++ [{ :hackney, github: "benoitc/hackney" }]
  end

  defp deps(_) do
    deps(:prod)
  end
end
