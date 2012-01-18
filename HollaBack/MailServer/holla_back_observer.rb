require 'json'

class HollaBackObserver
  MAIL_KEY = "hollaback:messages"
  class SimpleMail < Struct.new(:from, :to, :subject, :body, :offset_seconds)
    def to_json
      { :from           => from,
        :to             => to,
        :subject        => subject,
        :body           => body,
        :offset_seconds => offset_seconds }.to_json
    end
  end

  def initialize(redis)
    @redis = redis
  end

  def update(message_string)
    mail  = Mail.read_from_string(message_string)
    smail = SimpleMail.new(mail.from.first.to_s,
                           mail.to.first.to_s,
                           mail.subject.to_s,
                           mail.body.to_s,
                          offset_seconds(mail.date))
    puts "[#{Time.now.to_i}] Received mail from #{smail.from} to #{smail.to}"
    @redis.lpush(MAIL_KEY, smail.to_json)
  rescue => e
    $stderr.puts "ERROR: #{e.message}\n#{e.backtrace.join("\n")}"
  end

private

  def offset_seconds(time)
    hours, mins, secs = time.strftime("%::z").split(':', 3).map(&:to_i)
    hours * 3600 + mins * 60 + secs
  end
end
