defmodule ApathyDriveWeb.SessionController do
  use ApathyDriveWeb, :controller
  alias ApathyDrive.{Character, RaceTrait}

  plug(:scrub_params, "session" when action in [:create])

  def new(conn, _params) do
    changeset = Character.sign_up_changeset(%Character{})

    {races, traits} = races_and_traits()

    render(conn, "new.html",
      changeset: changeset,
      races: races,
      traits: traits,
      tab: "signin_form"
    )
  end

  def create(conn, %{"session" => %{"email" => nil}}) do
    email_or_password_incorrect(conn)
  end

  def create(conn, %{"session" => %{"password" => nil}}) do
    email_or_password_incorrect(conn)
  end

  def create(conn, %{"session" => %{"email" => email, "password" => password}}) do
    if character = Character.sign_in(email, password) do
      conn =
        conn
        |> put_session(:character, character.id)

      redirect(conn, to: Routes.game_path(conn, :game, %{}))
    else
      email_or_password_incorrect(conn)
    end
  end

  def delete(conn, _params) do
    conn
    |> put_session(:character, nil)
    |> redirect(to: "/")
  end

  defp email_or_password_incorrect(conn) do
    changeset = Character.sign_up_changeset(%Character{})

    {races, traits} = races_and_traits()

    conn
    |> put_flash(:sign_in, "email or password incorrect")
    |> render("new.html", races: races, traits: traits, changeset: changeset, tab: "signin_form")
  end

  def races_and_traits do
    races = ApathyDrive.Race.all()

    traits =
      Enum.reduce(races, %{}, fn race, traits ->
        list =
          race.id
          |> RaceTrait.load_traits()
          |> Enum.map(&ApathyDrive.Commands.Help.massage_trait(&1))
          |> List.flatten()
          |> Enum.reject(&is_nil/1)
          |> Enum.map(fn
            {name, nil} ->
              "<div>  <span class='dark-green'>#{name}</span></div>"

            {name, value} ->
              "<div>  <span class='dark-green'>#{name}:</span> <span class='dark-cyan'>#{value}</span></div>"
          end)

        list =
          if race.stealth do
            ["<span class='dark-green'>Racial Stealth</span>" | list]
          else
            list
          end
          |> Enum.join("\n")

        Map.put(traits, race.id, {:safe, list})
      end)

    {races, traits}
  end
end
