defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "0.0.1",
      elixir: "~> 1.0.0",
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
      {:cowboy,      "~> 1.0.0"},
      {:phoenix,     "~> 0.4.1"},
      {:ecto,        "~> 0.2.4"},
      {:postgrex,    "~> 0.6.0"},
      {:timex,       "~> 0.12.7"},
      {:inflex,      "~> 0.2.8"},
      {:block_timer, github: "adamkittelson/block_timer", ref: "f57fae9ea39fc43366e51063c840d409746ed67d"}
    ]
  end
end
