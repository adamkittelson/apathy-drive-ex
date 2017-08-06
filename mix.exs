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
      applications: [:logger, :rollbax, :postgrex, :ecto, :phoenix, :cowboy, :conform,
                     :oauth2, :phoenix_ecto, :comeonin, :runtime_tools, :csvlixir,
                     :tzdata, :timex_ecto, :scrivener, :scrivener_ecto, :ex_statsd, :ex_admin,
                     :phoenix_html, :inflex, :connection, :decimal, :neotoma, :phoenix_pubsub]
    ]
  end

  defp deps do
    [
      {:cowboy,              "~> 1.0.0"},
      {:ecto,                "~> 2.1", override: true},
      {:db_connection,       "~> 1.1"},
      {:postgrex,            "~> 0.13", override: true},
      {:phoenix,             "~> 1.2.0"},
      {:plug, "~>1.3.5", override: true},
      {:phoenix_pubsub,      "~> 1.0.0"},
      {:phoenix_live_reload, "~> 1.0.5", only: :dev},
      {:phoenix_ecto,        "~> 3.0.0"},
      {:phoenix_html,        "~> 2.6.0"},
      {:timex_ecto,          "~> 1.1.3"},
      {:inflex,              "~> 1.7"},
      {:oauth2,              "~> 0.5"},
      {:comeonin,            "~> 1.2.2"},
      {:conform,             "2.3.2"},
      {:ex_statsd,           "~> 0.5.3"},
      {:rollbax,             "~> 0.6.1"},
      {:scrivener_ecto,      "~> 1.0.0", override: true},
      {:distillery,          "~> 1.4.1", runtime: false},

      # fix compilation error with newer ecto
      {:ex_admin,            github: "smpallen99/ex_admin", ref: "490e32aafe40ffc3b60358e6b66c2e00a21ebedb"},

      {:mix_test_watch, "~> 0.4.0", only: :dev, runtime: false}
    ]
  end

  defp version do
    ~r/[0-9]+/
    |> Regex.scan(File.read!("VERSION.yml"))
    |> List.flatten
    |> Enum.join(".")
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/support"]
  defp elixirc_paths(_), do: ["lib", "web"]
end
