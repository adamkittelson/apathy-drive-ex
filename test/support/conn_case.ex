defmodule ApathyDriveWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  imports other functionality to make it easier
  to build and query models.

  Finally, if the test case interacts with the database,
  it cannot be async. For this reason, every test runs
  inside a transaction which is reset at the beginning
  of the test unless the test case is marked as async.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      use Phoenix.ConnTest

      # Alias the data repository and import query/model functions
      alias ApathyDrive.Repo
      import Ecto
      import Ecto.Query, only: [from: 2]

      # Import URL helpers from the router
      alias ApathyDriveWeb.Router.Helpers, as: Routes

      def session_conn() do
        opts =
          Plug.Session.init(
            store: :cookie,
            key: "foobar",
            encryption_salt: "encrypted cookie salt",
            signing_salt: "signing salt",
            log: false,
            encrypt: false
          )

        build_conn()
        |> Plug.Session.call(opts)
        |> fetch_session()
      end

      # The default endpoint for testing
      @endpoint ApathyDriveWeb.Endpoint
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(ApathyDrive.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(ApathyDrive.Repo, {:shared, self()})
    end

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
