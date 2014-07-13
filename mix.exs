defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "0.0.1",
      elixir: "~> 0.14.3",
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
      {:phoenix,  "0.3.0"},
      {:cowboy,   "~> 0.10.0", github: "extend/cowboy", optional: true},
      {:ecto,     "~> 0.2.2"},
      {:decimal,  "~> 0.2.2"},
      {:postgrex, "~> 0.5.2"},
      {:timex,    "~> 0.9.0"}
    ]
  end
end
