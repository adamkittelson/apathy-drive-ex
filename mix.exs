defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "0.0.1",
      elixir: "~> 0.15.0",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { ApathyDrive, [] },
      applications: [:phoenix]
    ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat" }
  defp deps do
    [
      {:cowboy, "~> 1.0.0"},
      {:phoenix,  github: "phoenixframework/phoenix", ref: "2a029dcd858fef17dd54bf3a435ae4fa15490edb"},
      {:ecto,     "~> 0.2.3"},
      {:decimal,  "~> 0.2.4"},
      {:postgrex, "~> 0.5.4"},
      {:timex,    "~> 0.11.0"},
      {:timer,    github: "adamkittelson/timer", ref: "93ddb3cd46a5faef7fd6becc4224797df807936d"}
    ]
  end
end
