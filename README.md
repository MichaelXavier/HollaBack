HollaBack
---------

HollaBack is an open-source email-based reminder system. When the mail server is configured, reminders can be registered via the mailbox name to which the email is sent. Here's some scenarios:

Scenario 1: BCC
===============
Bob emails you an important TPS report. You hate thinking about such things and
know that you only need a day to do it and that it must be dealt with by the
end of day on Friday. You can reply to Bob's email and bcc friday@example.com.
On friday, HollaBack will send you back an email with the full conversion. In
the mean time, you can clear the email out of sight and out of your brain.

Scenario 2: Direct Email
========================
An album is coming out soon that you must get your hands on. You don't want things like this in your calendar or tasks list because it is too much noise and isn't something you want to keep at the forefront of your mind. Effortless is the key. Email mar13@example.com wit the details and feel free to forget about it.

Requirements
============
Hollaback is primarily implemented with Haskell, but uses a Ruby mail server
implementation to parse requests.

Hollaback has the following system requirements:

1. A recent version of GHC. HollaBack is developed using GHC 7.0.3. It can
   proobably use GHC 6.x, but this is untested.
2. Ruby 1.8.7 or higher. 
3. The bundler gem. Use `gem install bundler` to install it.
4. Redis
5. sendmail

Building
========
Run `make build`.

Running
=======
To run the mail server, run `./bin/mail_server`

To run the scheduler, run `./bin/hollaback`

To run specs, run `make spec`

Status
======
This project is not nearly complete. Features implemented so far:

* Very simple email server using a vendored copy of rumbster.
* Scheduler which pulls emails off the queue at the appropriate time.
* Poller that transports messages from the mail server into the Redis backend.
* Date/time parsing that is almost 100% compatible with followup.cc, minus
  tags.
* All redis keys stored to the `hollaback:` namespace
* Mailer sends mail using sendmail without any configurable flags

Features yet to be implemented and pitfalls:

* Error handling is at an absolute minimum.
* Attachments are discarded (this probably won't change).
* No rate limiting or authentication to speak of.
* Need more introspection into the backend for troubleshooting.

Credits
=======
This service was inspired by [followup.cc](http://www.followup.cc) and aims to
be interface compatible with them at the most basic level. Their service is
excellent and far more full-featured than HollaBack. HollaBack is for my own
learning experience and for people who want to take followup.cc's concept to a
different level or for people who are concerned about privacy. If you want a
polished service that has lots of features, go sign up with them and support
them.

This project vendors the [rumbster](https://github.com/aesterline/rumbster)
project. rumbster is an SMTP server that allows you to register observers on
receipt of email.

The scheduling algorithm I used was a straight port from
[resque-scheduler](https://github.com/bvandenbos/resque-scheduler.git). I
thought the solution was elegant and simple. 
