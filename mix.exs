defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "#{version()}+#{build()}",
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
      applications: [:rollbax, :postgrex, :ecto, :phoenix, :cowboy, :conform, :conform_exrm,
                     :logger, :oauth2, :phoenix_ecto, :comeonin, :runtime_tools,
                     :tzdata, :timex_ecto, :scrivener, :phoenix_live_reload, :ex_statsd,
                     :phoenix_html, :inflex, :connection, :decimal, :neotoma, :phoenix_pubsub]
    ]
  end

  defp deps do
    [
      {:cowboy,              "~> 1.0.0"},
      {:ecto,                "2.0.0-rc.4", override: true},
      {:postgrex,            "~> 0.11.1"},
      {:phoenix,             "~> 1.2.0-rc"},
      {:phoenix_pubsub,      "~> 1.0.0-rc"},
      {:phoenix_live_reload, "~> 1.0.5"},
      {:phoenix_ecto,        "~> 3.0-rc"},
      {:phoenix_html,        "~> 2.5.1"},
      {:timex_ecto,          github: "bitwalker/timex_ecto"},
      {:inflex,              "~> 0.2.8"},
      {:oauth2,              "~> 0.5"},
      {:scrivener,           "~> 1.0"},
      {:comeonin,            "~> 1.2.2"},
      {:exrm,                "1.0.3"},
      {:conform,             "2.0.0"},
      {:conform_exrm,        "1.0.0"},
      {:ex_statsd,           "~> 0.5.3"},
      {:rollbax,             "~> 0.5"},
      {:shouldi,             "0.3.0", only: :test}
    ]
  end

  defp version do
    ~r/[0-9]+/
    |> Regex.scan(File.read!("VERSION.yml"))
    |> List.flatten
    |> Enum.join(".")
  end

  defp build do
    case System.cmd("ls", ["rel/apathy_drive/releases"]) do
      {"", _exit_status} ->
        "00000"
      {result, 0} ->
        result
        |> String.split("\n")
        |> Enum.filter(&(&1 =~ ~r/\d+\.\d+\.\d+/))
        |> List.last
        |> build()
    end
  end

  defp build(nil), do: "00000"
  defp build(version) do
    case String.split(version, "+") do
      [_version] ->
        "00000"
      [_version, build] ->
        build
        |> String.to_integer
        |> +(1)
        |> to_string
        |> String.rjust(5, ?0)
    end
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/matchers", "test/support"]
  defp elixirc_paths(_), do: ["lib", "web"]
end
