defmodule ApathyDrive.ChannelCase do
  @moduledoc """
  This module defines the test case to be used by
  channel tests.

  Such tests rely on `Phoenix.ChannelTest` and also
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
      # Import conveniences for testing with channels
      use Phoenix.ChannelTest
      alias ApathyDrive.Repo
      import Ecto
      import Ecto.Changeset
      import Ecto.Query

      # The default endpoint for testing
      @endpoint ApathyDrive.Endpoint

      def test_mobile(map \\ %{}, room \\ Repo.insert!(%Room{})) do
        class = Repo.insert!(%ApathyDrive.Class{alignment: "evil",
                                                name: "Demon",
                                                start_room_id: room.id,
                                                abilities: [],
                                                strength: 40,
                                                strength_per_level: 5,
                                                agility: 40,
                                                agility_per_level: 5,
                                                will: 40,
                                                will_per_level: 5,
                                                unities: ["evil"]})

        # Hack to prevent it from trying to load rooms that don't exist
        Task.start(fn ->
          :global.register_name("room_#{room.id}", self())
          receive do end
        end)

        spirit =
          %Spirit{room_id: room.id, class_id: class.id}
          |> Map.merge(map)
          |> Repo.insert!

          token = Phoenix.Token.sign(ApathyDrive.Endpoint, "spirit", spirit.id)

          {:ok, _, socket} = socket() |> subscribe_and_join(ApathyDrive.MUDChannel, "mud:play", %{"spirit" => token})

          socket.assigns[:mobile]
      end
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(ApathyDrive.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(ApathyDrive.Repo, {:shared, self()})
    end

    :ok
  end
end
