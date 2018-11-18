defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [
      app: :apathy_drive,
      version: version(),
      elixir: "~> 1.7.4",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix] ++ Mix.compilers(),
      deps: deps(),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod
    ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: {ApathyDrive, []},
      extra_applications: extra_applications()
    ]
  end

  defp extra_applications() do
    extra_apps = [:logger, :runtime_tools]

    if Application.get_env(:logger, :handle_sasl_reports) do
      [:sasl | extra_apps]
    else
      extra_apps
    end
  end

  defp deps do
    [
      {:cowboy, "~> 1.0.0"},
      {:ecto, "~> 2.1", override: true},
      {:db_connection, "~> 1.1"},
      {:postgrex, "~> 0.13", override: true},
      {:phoenix, "~> 1.3.0"},
      {:phoenix_pubsub, "~> 1.0.0"},
      {:phoenix_live_reload, "~> 1.1.0", only: :dev},
      {:phoenix_ecto, "~> 3.2.0"},
      {:phoenix_html, "~> 2.10.0"},
      {:scrivener_ecto, "~> 1.3.0"},
      {:timex_ecto, "~> 3.1.1"},
      {:inflex, "~> 1.7"},
      {:comeonin, "~> 3.2.0"},
      {:conform, "~> 2.5.0"},
      {:statix, "~> 1.1.0"},
      {:rollbax, "~> 0.8.2"},
      {:gossip, "~> 1.0.0"},
      {:distillery, "~> 1.5.5", runtime: false},
      {:mix_test_watch, "~> 0.4.0", only: :dev, runtime: false}
    ]
  end

  defp version do
    ~r/[0-9]+/
    |> Regex.scan(File.read!("VERSION.yml"))
    |> List.flatten()
    |> Enum.join(".")
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
