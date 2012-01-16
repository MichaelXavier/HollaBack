$:.unshift(File.join(File.dirname(__FILE__), '..', 'lib'))

require 'test/unit'
require 'stringio'
require 'smtp_protocol'

class TestSmtpStates < Test::Unit::TestCase
 
  def setup
    buffer = ''
    @server_stream = StringIO.new(buffer)
    @client_stream = StringIO.new(buffer)    
    @protocol = TestProtocol.new
  end
 
  def test_greeting_is_returned_upon_initial_client_connection
    init_state = InitState.new
    init_state.serve(@server_stream)
       
    assert_equal "220 ruby ESMTP\n", @client_stream.readline
  end
  
  def test_initial_state_passes_protocol_connect_as_the_next_state_in_the_chain
    init_state = InitState.new
    next_state = init_state.serve(@server_stream)
    
    assert_equal :connect, next_state 
  end
  
  def test_helo_is_accepted_while_in_connect_state
    @client_stream.puts "HELO test.client"
    connect_state = ConnectState.new

    connect_state.serve(@server_stream)
    
    assert_equal "250 ruby\n", @client_stream.readline
  end
  
  def test_connect_state_passes_protocol_connected_as_the_next_state_in_the_chain
    @client_stream.puts "HELO test.client"
    connect_state = ConnectState.new

    next_state = connect_state.serve(@server_stream)
    
    assert_equal :connected, next_state 
  end
  
  def test_from_okayed_while_in_connected_state
    @client_stream.puts "MAIL FROM:<adam@esterlines.com>"
    connected_state = ConnectedState.new

    connected_state.serve(@server_stream)
    
    assert_equal "250 ok\n", @client_stream.readline
  end
  
  def test_connected_state_passes_protocol_connected_as_the_next_state_when_client_sends_from_request
    @client_stream.puts "MAIL FROM:<junk@junkster.com>"
    connected_state = ConnectedState.new

    next_state = connected_state.serve(@server_stream)
    
    assert_equal :connected, next_state 
  end
  
  def test_rcpt_okayed_while_in_connected_state
    @client_stream.puts "RCPT TO:<junk@junkster.com>"    
    connected_state = ConnectedState.new

    connected_state.serve(@server_stream)
    
    assert_equal "250 ok\n", @client_stream.readline
  end
  
  def test_connected_state_passes_protocol_connected_as_the_next_state_when_client_sends_rcpt_request
    @client_stream.puts "RCPT TO:<foo@foo.com>"    
    connected_state = ConnectedState.new

    next_state = connected_state.serve(@server_stream)
    
    assert_equal :connected, next_state 
  end
  
  def test_data_request_given_the_go_ahead_while_in_connected_state
    @client_stream.puts "DATA"
    connected_state = ConnectedState.new

    connected_state.serve(@server_stream)
    
    assert_equal "354 go ahead\n", @client_stream.readline
  end
  
  def test_connected_state_passes_protocol_read_mail_as_the_next_state_when_client_sends_data_request
    @client_stream.puts "DATA"
    connected_state = ConnectedState.new

    next_state = connected_state.serve(@server_stream)
    
    assert_equal :read_mail, next_state 
  end
  
  def test_read_mail_state_reads_until_a_single_dot_is_found_on_a_line_then_returns_an_ok_message
    @client_stream.puts "To: junk@junk.com\nFrom: junk2@junk2.com\n\nHi\n.\n"
    read_mail_state = ReadMailState.new(@protocol)
    
    read_mail_state.serve(@server_stream)
    
    assert_equal "250 ok\n", @client_stream.readline
  end
  
  def test_read_mail_state_passes_read_message_to_protocol
    message = "To: junk@junk.com\nFrom: junk2@junk2.com\n\nHi\n"
    @client_stream.puts "#{message}.\n"
    read_mail_state = ReadMailState.new(@protocol)
    
    read_mail_state.serve(@server_stream)
    
    assert_equal message, @protocol.new_message
  end
  
  def test_read_mail_state_passes_protocol_quit_as_the_next_state_when_mail_message_is_read
    @client_stream.puts "To: junk@junk.com\nFrom: junk2@junk2.com\n\nHi\n.\n"
    read_mail_state = ReadMailState.new(@protocol)
    
    next_state = read_mail_state.serve(@server_stream)
    
    assert_equal :quit, next_state 
  end
  
  def test_quit_state_reads_client_quit_and_says_goodbye
    @client_stream.puts "QUIT"
    quit_state = QuitState.new
    
    quit_state.serve(@server_stream)
    
    assert_equal "221 ruby goodbye\n", @client_stream.readline
  end

  def test_quit_state_passes_done_as_next_state
    @client_stream.puts "QUIT"
    quit_state = QuitState.new
    
    next_state = quit_state.serve(@server_stream)
  
    assert_equal :done, next_state
  end
  
end

class TestProtocol
  attr_accessor :state, :io
  attr_reader :new_message
  
  def serve (io)
    @io = io
  end
  
  def new_message_received(message)
    @new_message = message
  end
end
