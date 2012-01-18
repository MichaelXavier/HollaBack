module HollaBack.Mailer (hollaBack) where

import Data.Text.Lazy (empty,
                       fromStrict)
import Network.Mail.Mime (Mail,
                          Address(..),
                          renderSendMail,
                          simpleMail)

import HollaBack.Types

hollaBack :: Payload -> IO ()
hollaBack pl = renderSendMail =<< renderPayload pl

---- Helpers
renderPayload :: Payload -> IO Mail 
renderPayload Payload { to      = t,
                        from    = f,
                        subject = s,
                        body    = b} = simpleMail t' f' s b' empty []
  where t'        = toAddress t
        f'        = toAddress f
        b'        = fromStrict b
        toAddress = Address Nothing
