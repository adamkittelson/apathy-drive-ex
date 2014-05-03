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
      { :weber,    github: "elixir-web/weber", ref: "3132e10754b7ce8942b13eb08b34e55009592c6d" },
      { :ecto,     github: "elixir-lang/ecto", ref: "303131c50b5881fb7836ddbe2e88e0241c5e83c6" },
      { :exjson,   github: "adamkittelson/exjson", ref: "0745ea9eb3af3105ad9235907fd51620fa927709", override: true},
      { :postgrex, "~> 0.4.2"},
      { :bcrypt,   github: "Feuerlabs/erlang-bcrypt"}
    ]
  end

  defp deps(:test) do
    deps(:prod) ++ [{ :hackney, github: "benoitc/hackney" }]
  end

  defp deps(_) do
    deps(:prod)
  end
end
