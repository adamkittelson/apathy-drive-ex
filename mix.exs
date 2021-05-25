defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [
      app: :apathy_drive,
      version: version(),
      elixir: "~> 1.12.0",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix] ++ Mix.compilers(),
      deps: deps(),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      releases: releases()
    ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: {ApathyDrive, []},
      extra_applications: extra_applications()
    ]
  end

  defp releases() do
    [
      apathy_drive: [
        steps: [:assemble, &make_tarball/1],
        config_providers: [
          {Toml.Provider,
           [
             path: {:system, "RELEASE_CONFIG_DIR", "apathy_drive.toml"},
             transforms: []
           ]}
        ]
      ]
    ]
  end

  defp make_tarball(release) do
    File.cd("/usr/src/app/_build/prod/rel/apathy_drive")

    System.cmd("tar", [
      "czf",
      "/usr/src/app/_build/prod/rel/apathy_drive/apathy_drive.tar.gz",
      "."
    ])

    release
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
      {:jason, "~> 1.2.0"},
      {:postgrex, "~> 0.13"},
      {:phoenix, "~> 1.5.1"},
      {:phoenix_pubsub, "~> 2.0"},
      {:phoenix_live_reload, "~> 1.3.1", only: :dev},
      {:fs, "~> 6.12.0"},
      {:phoenix_live_view, "~> 0.12.1"},
      {:ecto_sql, "~> 3.4.3"},
      {:phoenix_ecto, "~> 4.1.0"},
      {:plug_cowboy, "~> 2.1"},
      {:plug, "~> 1.7"},
      {:scrivener_ecto, "~> 2.1.1"},
      {:inflex, "~> 1.7"},
      {:comeonin, "~> 3.2.0"},
      {:statix, "~> 1.1.0"},
      {:rollbax, "~> 0.8.2"},
      {:gossip, "~> 1.4.0"},
      {:distillery, "~> 2.0.12"},
      {:toml,
       github: "adamkittelson/toml-elixir", ref: "4c0b24a2b409f6bdec78876c1324eea16f74a093"},
      {:auto_linker, "~> 0.2.2"},
      {:ordinal, "~> 0.1.0"},
      {:dnsimple, "~> 1.4.0"},
      {:tzdata, "~> 1.0.1"},
      {:timex, "~> 3.6.1"},
      {:logflare_logger_backend, "~> 0.7.6"},
      {:floki, ">= 0.0.0", only: :test}
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
