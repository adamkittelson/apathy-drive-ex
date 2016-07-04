defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: version(),
      elixir: "~> 1.3.0",
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
      applications: [:logger, :rollbax, :postgrex, :ecto, :phoenix, :cowboy, :conform,
                     :conform_exrm, :oauth2, :phoenix_ecto, :comeonin, :runtime_tools,
                     :tzdata, :timex_ecto, :scrivener, :ex_statsd,
                     :phoenix_html, :inflex, :connection, :decimal, :neotoma, :phoenix_pubsub]
    ]
  end

  defp deps do
    [
      {:cowboy,              "~> 1.0.0"},
      {:ecto,                "2.0.2", override: true},
      {:postgrex,            "~> 0.11.2"},
      {:phoenix,             "~> 1.2.0"},
      {:phoenix_pubsub,      "~> 1.0.0"},
      {:phoenix_live_reload, "~> 1.0.5", only: :dev},
      {:phoenix_ecto,        "~> 3.0.0"},
      {:phoenix_html,        "~> 2.6.0"},
      {:timex_ecto,          "~> 1.1.3"},
      {:inflex,              "~> 1.7"},
      {:oauth2,              "~> 0.5"},
      {:comeonin,            "~> 1.2.2"},
      {:exrm,                "1.0.3"},
      {:conform,             "2.0.0"},
      {:conform_exrm,        "1.0.0"},
      {:ex_statsd,           "~> 0.5.3"},
      {:rollbax,             "~> 0.6.1"},
      {:ex_admin, github: "smpallen99/ex_admin"},
      {:shouldi,             "0.3.0", only: :test}
    ]
  end

  defp version do
    ~r/[0-9]+/
    |> Regex.scan(File.read!("VERSION.yml"))
    |> List.flatten
    |> Enum.join(".")
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/matchers", "test/support"]
  defp elixirc_paths(_), do: ["lib", "web"]
end
