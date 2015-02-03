defmodule ApathyDrive.Matchers.Socket do
  import ExUnit.Assertions
  import ShouldI.Matcher

  defmatcher should_add_to_scroll(string, body) do
    quote do

      unquote(body[:do])

      assert_received {:socket_reply,
                       %Phoenix.Socket.Message{event: "scroll",
                                               payload: %{
                                                 html: unquote(string)},
                                                 topic: nil}}

    end
  end

  defmacro adds_to_scroll(string) do
    quote do
      assert_received {:socket_reply,
                       %Phoenix.Socket.Message{event: "scroll",
                                               payload: %{
                                                 html: unquote(string)},
                                                 topic: nil}}
    end
  end
end
