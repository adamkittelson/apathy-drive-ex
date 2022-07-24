defmodule ApathyDrive.Mailer do
  use Bamboo.Mailer, otp_app: :apathy_drive
  import Bamboo.Email

  alias ApathyDrive.Repo

  def send_welcome_email(character) do
    token = Phoenix.Token.sign(ApathyDriveWeb.Endpoint, "welcome email", character.id)

    character
    |> Ecto.Changeset.change(%{
      welcome_token: token
    })
    |> Repo.update!()

    url = "#{ApathyDriveWeb.Endpoint.url()}/welcome/#{token}"

    new_email()
    |> from("noreply@apotheos.is")
    |> to(character.email)
    |> Bamboo.SendGridHelper.with_template("d-68df002c0b934ee1badf4b8349edcfc0")
    |> Bamboo.SendGridHelper.add_dynamic_field("Weblink", url)
    |> ApathyDrive.Mailer.deliver_now!()
  end
end
