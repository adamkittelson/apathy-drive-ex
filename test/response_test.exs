defmodule WeberHttpResponseTest do
  use ExUnit.Case

  test "SimpleResponse test" do
    {:ok, status, _, client} = :hackney.request(:get, 'http://localhost:3000/weber', [], <<>>, [])
    assert(status == 404)
  end

end