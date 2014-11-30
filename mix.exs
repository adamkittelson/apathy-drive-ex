defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "0.0.1",
      elixir: "~> 1.0.2",
      elixirc_paths: ["lib", "web"],
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
      {:phoenix,     "~> 0.6.1"},
      {:ecto,        "~> 0.2.4"},
      {:postgrex,    "~> 0.6.0"},
      {:timex,       "~> 0.12.7"},
      {:inflex,      "~> 0.2.8"},
      {:block_timer, "~> 0.0.1"}
    ]
  end
end
