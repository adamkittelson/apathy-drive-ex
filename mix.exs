defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "0.0.1",
      elixir: "~> 1.0.2",
      elixirc_paths: elixirc_paths(Mix.env),
      compilers: [:phoenix] ++ Mix.compilers,
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { ApathyDrive, [] },
      applications: [:phoenix, :cowboy, :logger]
    ]
  end

  defp deps do
    [
      {:cowboy,      "~> 1.0.0"},
      {:phoenix,     "~> 0.8.0"},
      {:ecto,        "~> 0.4.0"},
      {:postgrex,    "~> 0.6.0"},
      {:timex,       "~> 0.12.7"},
      {:inflex,      "~> 0.2.8"},
      {:block_timer, "~> 0.0.1"},
      {:shouldi, only: :test}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/matchers"]
  defp elixirc_paths(_), do: ["lib", "web"]
end
