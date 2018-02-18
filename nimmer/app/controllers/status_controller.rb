require 'coap'
require 'json'
COAP_ADDRESS = "coap://10.200.19.31"

class StatusController < ApplicationController
  def index
		@currentLevel = jsonFromCoap('/weight')["weight"]
		@isEmpty = jsonFromCoap('/isEmpty')["isEmpty"] == "true"
		@gps = jsonFromCoap('/gps')["getGPS"]
		@calibrated = jsonFromCoap('/calibrated')["calibrated"]
		@serverAdress = jsonFromCoap('/serverAdress')["getServerAdress"]
		puts "\t\tHIER PASSIEREN DINGE"

		callback = ->(socket, message) do
			puts "\t\tCALL BACK !!! " + message.payload
		end
		begin
			CoAP::Client.new.get_by_uri(COAP_ADDRESS + '/observe')
		rescue Interrupt
			exit
		end
		puts "\t\tEND OF EVIL"
  end

	def calibrate_empty()
		puts "\t\tCALIBRATE EMPTY"
		CoAP::Client.new.post_by_uri(COAP_ADDRESS + '/calibrate', "true")
	end
	
	def calibrate_full()
		puts "\t\tCALIBRATE EMPTY"
		CoAP::Client.new.post_by_uri(COAP_ADDRESS + '/calibrate', "false")
	end

  private
def jsonFromCoap(uri)
  	JSON.parse(CoAP::Client.new.get_by_uri(COAP_ADDRESS + uri).payload)
	end
end
