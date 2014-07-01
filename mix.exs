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
      { :weber,    github: "elixir-web/weber", ref: "3399cab61481de794ca6af53eb4bdf45d94b5e3c" },
      { :weberContrib, github: "elixir-web/weber-contrib", ref: "2d64328195408a0d690c64f4d4b5055581cffe23", override: true },
      { :exjson,   github: "adamkittelson/exjson", ref: "5c51b256976d7e71e8638a7895423f58b5dd4b25", override: true },
      { :ecto,     "~> 0.2.2" },
      { :plug,     "~> 0.5.1" },
      { :jazz,     "~> 0.1.2" },
      { :postgrex, "~> 0.5.2" },
      { :decimal,  "~> 0.2.2" },
      { :bcrypt,   github: "Feuerlabs/erlang-bcrypt"},
      { :inflex,   "~> 0.2.4" }
    ]
  end

  defp deps(:test) do
    deps(:prod)
  end

  defp deps(_) do
    deps(:prod)
  end
end
