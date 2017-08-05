defmodule SpiritTest do
  use ApathyDrive.ModelCase

  @valid_attrs %{email: "adam@apathydrive.com",
                 password: "awesome password",
                 password_confirmation: "awesome password"}

  @invalid_email %{email: "adamapathydrive.com",
                   password: "awesome password",
                   password_confirmation: "awesome password"}

  @short_password %{email: "adam@apathydrive.com",
                    password: "pswd",
                    password_confirmation: "pswd"}

  @pw_confirmation %{email: "adam@apathydrive.com",
                     password: "awesome password",
                     password_confirmation: "awesome pw"}

  test "sign_up changeset with valid attributes" do
    changeset = Spirit.sign_up_changeset(%Spirit{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with an invalid email address" do
    changeset = Spirit.sign_up_changeset(%Spirit{}, @invalid_email)
    assert changeset.errors == [email: {"has invalid format", [validation: :format]}]
    refute changeset.valid?
  end

  test "changeset with a password that is too short" do
    changeset = Spirit.sign_up_changeset(%Spirit{}, @pw_confirmation)
    assert changeset.errors == [password_confirmation: {"does not match confirmation", [validation: :confirmation]}]
    refute changeset.valid?
  end
end
