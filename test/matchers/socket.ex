defmodule ApathyDrive.Matchers do
  import ExUnit.Assertions
  import ShouldI.Matcher

  defmatcher should_add_to_scroll(string, body) do
    quote do

      unquote(body[:do])

      assert_received {:socket_push,
                       %Phoenix.Socket.Message{event: "scroll",
                                               payload: %{
                                                 html: unquote(string)},
                                                 topic: nil}}

    end
  end

  defmacro assert_adds_to_scroll(string) do
    quote do
      assert_received {:socket_push,
                       %Phoenix.Socket.Message{event: "scroll",
                                               payload: %{
                                                 html: unquote(string)},
                                                 topic: nil}}
    end
  end

  defmacro assert_lists_match(list1, list2) do
    quote do
      list1 = unquote(list1)
              |> Enum.map(&(&1.id))
              |> Enum.sort

      list2 = unquote(list2)
              |> Enum.map(&(&1.id))
              |> Enum.sort
      assert list1 == list2
    end
  end
end
