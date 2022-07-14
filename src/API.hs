module API (api) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Server (httpPlayground)
import Data.Morpheus.Types
  ( GQLType,
    ResolverQ,
    RootResolver (queryResolver),
    Undefined,
    defaultRootResolver,
    liftEither,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.Scotty (body, get, post, raw, scotty)

data Query m = Query
  { deity :: DeityArgs -> m Deity
  }
  deriving (Generic, GQLType)

data Deity = Deity
  { fullName :: Text,
    power :: Maybe Text
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text,
    mythology :: Maybe Text
  }
  deriving (Generic, GQLType)

resolveDeity :: DeityArgs -> ResolverQ e IO Deity
resolveDeity DeityArgs {name, mythology} = liftEither $ dbDeity name mythology

dbDeity :: Text -> Maybe Text -> IO (Either String Deity)
dbDeity "Migas" mName = pure (Right (Deity "Migas" (Just "Fode fofo")))
dbDeity "Guizinho" mName = pure (Right (Deity "Guizinho" (Just "Soca fofo")))
dbDeity name mName = pure (Left "nao encontrado")

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query {deity = resolveDeity}
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver

api :: IO ()
api = scotty 3000 $ do
  post "/api" $ raw =<< (liftIO . gqlApi =<< body)
  get "/" (raw httpPlayground)
