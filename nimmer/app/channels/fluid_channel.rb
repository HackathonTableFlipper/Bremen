class FluidChannel < ApplicationCable::Channel
  def subscribed
		stream_from "fluid"
  end

  def unsubscribed
    # Any cleanup needed when channel is unsubscribed
  end
end
