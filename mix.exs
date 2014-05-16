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
      { :weber,    github: "elixir-web/weber", ref: "a910311a8be63fc210540de03691a596d662b97c" },
      { :ecto,     github: "elixir-lang/ecto", ref: "303131c50b5881fb7836ddbe2e88e0241c5e83c6" },
      { :jazz,     "~> 0.1.0"},
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
