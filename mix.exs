defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: version(),
      elixir: "~> 1.5.1",
      elixirc_paths: elixirc_paths(Mix.env),
      compilers: [:phoenix] ++ Mix.compilers,
      deps: deps(),
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { ApathyDrive, [] },
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  defp deps do
    [
      {:cowboy,              "~> 1.0.0"},
      {:ecto,                "~> 2.1", override: true},
      {:db_connection,       "~> 1.1"},
      {:postgrex,            "~> 0.13", override: true},
      {:phoenix,             "~> 1.3.0"},
      {:plug, "~>1.3.5", override: true},
      {:phoenix_pubsub,      "~> 1.0.0"},
      {:phoenix_live_reload, "~> 1.0.5", only: :dev},
      {:phoenix_ecto,        "~> 3.2.0"},
      {:phoenix_html,        "~> 2.9.0"},
      {:timex_ecto,          "~> 3.1.1"},
      {:inflex,              "~> 1.7"},
      {:comeonin,            "~> 3.2.0"},
      {:conform,             "~> 2.5.0"},
      {:ex_statsd,           "~> 0.5.3"},
      {:rollbax,             "~> 0.6.1"},
      {:distillery,          "~> 1.4.1", runtime: false},
      {:mix_test_watch, "~> 0.4.0", only: :dev, runtime: false}
    ]
  end

  defp version do
    ~r/[0-9]+/
    |> Regex.scan(File.read!("VERSION.yml"))
    |> List.flatten
    |> Enum.join(".")
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
