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
      {:postgrex, "~> 0.15.9"},
      {:phoenix, "~> 1.5.9"},
      {:phoenix_pubsub, "~> 2.0"},
      {:phoenix_live_reload, "~> 1.3.1", only: :dev},
      {:fs, "~> 6.12.0"},
      {:phoenix_live_view, "~> 0.15.7"},
      {:ecto_sql, "~> 3.6.1"},
      {:phoenix_ecto, "~> 4.2.1"},
      {:plug_cowboy, "~> 2.5.0"},
      {:plug, "~> 1.11.1"},
      {:scrivener_ecto, "~> 2.7.0"},
      {:inflex, "~> 2.1.0"},
      {:comeonin, "~> 5.3.2"},
      {:bcrypt_elixir, "~> 2.0"},
      {:statix, "~> 1.4.0"},
      {:rollbax, "~> 0.11.0"},
      {:gossip, "~> 1.4.0"},
      {:toml,
       github: "adamkittelson/toml-elixir", ref: "4c0b24a2b409f6bdec78876c1324eea16f74a093"},
      {:ordinal, "~> 0.2.0"},
      {:dnsimple, "~> 3.0.0"},
      {:tzdata, "~> 1.1.0"},
      {:timex, "~> 3.7.5"},
      {:logflare_logger_backend, "~> 0.8.0"},
      {:floki, ">= 0.0.0", only: :test},
      # for tzdata
      {:hackney, "~> 1.0"}
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
