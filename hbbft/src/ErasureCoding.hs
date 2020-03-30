module ErasureCoding
  ( encodeByteString
  , decodeMessage
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.SecretSharing as SS
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)

encodeByteString :: MonadIO m => Int -> Int -> ByteString -> m [SS.Share]
encodeByteString m n msg = liftIO $ SS.encode m n (fromStrict msg)

decodeMessage :: MonadIO m => [SS.Share] -> m ByteString
decodeMessage = pure . toStrict . SS.decode
