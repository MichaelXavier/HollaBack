require 'observer'
require 'smtp_states'

class SmtpProtocol
  include Observable
  
  def SmtpProtocol.create
    initial_state = :init
    
    states = {
      :init => InitState.new, 
      :connect => ConnectState.new, 
      :connected => ConnectedState.new, 
      :read_mail => ReadMailState.new, 
      :quit => QuitState.new 
    }
    
    SmtpProtocol.new(initial_state, states)
  end

  def initialize(initial_state, states)
    states[:read_mail].protocol = self unless states[:read_mail].nil?

    @states = states
  end
  
  def serve(io)
    next_state = :init
    count = 0
    until next_state == :done or count > 10 do 
      next_state = @states[next_state].serve(io)
      count += 1
    end
  end
  
  def new_message_received(message)
    changed
    notify_observers(message)
  end
  
end

