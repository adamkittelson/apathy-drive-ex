defmodule ApathyDrive.Mixfile do
  use Mix.Project

  def project do
    [ app: :apathy_drive,
      version: "#{version()}-#{build()}",
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
      applications: [:postgrex, :ecto, :phoenix, :cowboy, :conform, :conform_exrm,
                     :logger, :oauth2, :phoenix_ecto, :comeonin, :runtime_tools,
                     :tzdata, :timex_ecto, :scrivener, :phoenix_live_reload,
                     :phoenix_html, :inflex, :connection, :decimal, :neotoma]
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
      {:exrm,                "1.0.0-rc7"},
      {:conform,             "1.0.0-rc8"},
      {:conform_exrm,        "0.2.0"},
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
    {result, _exit_code} = System.cmd("ls", ["rel/apathy_drive/releases"])

    result
    |> String.split("\n")
    |> Enum.filter(&(&1 =~ ~r/\d+\.\d+\.\d+/))
    |> List.last
    |> build()
  end

  defp build(nil), do: 0
  defp build(version) do
    case String.split(version, "-") do
      [_version] -> 0
      [_version, build] -> String.to_integer(build) + 1
    end
  end

  defp elixirc_paths(:test), do: ["lib", "web", "test/matchers", "test/support"]
  defp elixirc_paths(_), do: ["lib", "web"]
end
