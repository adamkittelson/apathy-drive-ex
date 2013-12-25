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
      { :weber,  github: "0xAX/weber" },
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
