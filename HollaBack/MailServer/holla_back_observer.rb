require 'json'

class HollaBackObserver
  MAIL_KEY = "followup:messages"
  class SimpleMail < Struct.new(:from, :to, :subject, :body)
    def to_json
      { :from    => from,
        :to      => to,
        :subject => subject,
        :body    => body }.to_json
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
                           mail.body.to_s)
    @redis.lpush(MAIL_KEY, smail.to_json)
  rescue => e
    $stderr.puts "ERROR: #{e.message}\n#{e.backtrace.join("\n")}"
  end
end
