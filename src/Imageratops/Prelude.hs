module Imageratops.Prelude
  ( module X
  , module Imageratops.Prelude
  ) where

import Prelude as X

import Control.Applicative          as X
import Control.Category             as X (Category, (<<<), (>>>))
import Control.DeepSeq              as X (NFData, deepseq, ($!!))
import Control.Exception.Safe       as X
  (Exception, bracket, bracket_, catch, handle, throwM, throwString, try)
import Control.Lens                 as X
  (Lens, Lens', lens, makeLenses, over, set, view, (.~), (?~), (^.))
import Control.Monad.Catch          as X (MonadCatch(), MonadThrow())
import Control.Monad.Except         as X
  ( Except
  , ExceptT(..)
  , MonadError(catchError, throwError)
  , mapExcept
  , mapExceptT
  , runExcept
  , runExceptT
  , withExcept
  , withExceptT
  )
import Control.Monad.IO.Class       as X (MonadIO(liftIO))
import Control.Monad.Reader         as X
  ( MonadReader(ask)
  , Reader
  , ReaderT(..)
  , asks
  , mapReader
  , mapReaderT
  , runReader
  , runReaderT
  )
import Control.Monad.Trans.Class    as X (MonadTrans(lift))
import Control.Monad.Trans.Resource as X
  (MonadResource(..), ResourceT, runResourceT)
import Control.Monad.Writer         as X
  ( MonadWriter(tell)
  , Writer
  , WriterT(..)
  , mapWriter
  , mapWriterT
  , runWriter
  , runWriterT
  )
import Data.Aeson                   as X (FromJSON(..), ToJSON(..))
import Data.Bifunctor               as X (Bifunctor(..), first, second)
import Data.ByteString              as X (ByteString)
import Data.Foldable                as X (asum)
import Data.Function                as X ((&))
import Data.Hashable                as X (Hashable)
import Data.Maybe                   as X (fromMaybe, isJust, isNothing)
import Data.Monoid                  as X ((<>))
import Data.Proxy                   as X (Proxy(Proxy))
import Data.String                  as X (IsString(..))
import Data.Text                    as X (Text)
import Data.UUID                    as X (UUID)
import GHC.TypeLits                 as X
import Network.AWS                  as X (MonadAWS)
import Text.Read                    as X (readEither, readMaybe)
import Web.HttpApiData              as X
  (FromHttpApiData(..), ToHttpApiData(..))

import qualified Data.ByteString.Lazy

type LByteString = Data.ByteString.Lazy.ByteString
