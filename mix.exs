defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "0.0.1",
      elixir: "~> 1.2.0",
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
      applications: [:postgrex, :ecto, :phoenix, :cowboy, :logger, :oauth2, :phoenix_ecto, :comeonin, :tzdata]
    ]
  end

  defp deps do
    [
      {:cowboy,              "~> 1.0.0"},
      {:ecto,                "~> 1.1"},
      {:postgrex,            "~> 0.10"},
      {:phoenix,             "~> 1.1"},
      {:phoenix_live_reload, "~> 1.0.1"},
      {:phoenix_ecto,        "~> 2.0"},
      {:phoenix_html,        "~> 2.3"},
      {:timex,               "~> 1.0.0-rc4"},
      {:timex_ecto,          "~> 0.7.0"},
      {:inflex,              "~> 0.2.8"},
      {:oauth2,              "~> 0.5"},
      {:scrivener,           "~> 1.0"},
      {:comeonin,            "~> 1.2.2"},
      {:shouldi, git: "https://github.com/batate/shouldi", only: :test}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/matchers", "test/support"]
  defp elixirc_paths(_), do: ["lib", "web"]
end
