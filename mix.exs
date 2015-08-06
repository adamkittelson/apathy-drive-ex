defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "0.0.1",
      elixir: "~> 1.0.4",
      elixirc_paths: elixirc_paths(Mix.env),
      compilers: [:phoenix] ++ Mix.compilers,
      deps: deps,
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { ApathyDrive, [] },
      applications: [:postgrex, :ecto, :phoenix, :cowboy, :logger, :oauth2, :phoenix_ecto, :comeonin]
    ]
  end

  defp deps do
    [
      {:cowboy,              "~> 1.0.0"},
      {:ecto,                "~> 0.14.3"},
      {:decimal,             "~> 1.1.0"},
      {:postgrex,            "~> 0.9.1"},
      {:phoenix,             "~> 0.16"},
      {:phoenix_live_reload, "~> 0.6", only: :dev},
      {:phoenix_ecto,        "~> 0.9"},
      {:phoenix_html,        "~> 2.0"},
      {:timex,               "~> 0.16"},
      {:timex_ecto,          "~> 0.4.0"},
      {:inflex,              "~> 0.2.8"},
      {:block_timer,         "~> 0.0.1"},
      {:oauth2,              "~> 0.0.5"},
      {:scrivener,           "~> 0.11.0"},
      {:comeonin,            "~> 1.0.5"},
      {:shouldi, only: :test}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/matchers", "test/support"]
  defp elixirc_paths(_), do: ["lib", "web"]
end
