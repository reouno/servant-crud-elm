{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module SampleServer where

import           Control.Monad.IO.Class ( liftIO )
import           Data.Aeson             ( FromJSON, ToJSON )
import           Data.Time              ( UTCTime )
import           Database.Persist
import           Elm.Derive             ( defaultOptions, deriveBoth )
import           GHC.Generics           ( Generic )
import           Servant
import           ServantCrud

data DatumType
  = TText
  | TImage
  | TAudio
  | TVideo
  deriving (Eq, Show)

--instance FromJSON DatumType
--instance ToJSON DatumType
data Datum =
  Datum
    { datumName      :: String
    , datumType      :: DatumType
    , datumContent   :: String
    , datumCreatedAt :: UTCTime
    }
  deriving (Eq, Show)

-- instance FromJSON Datum
-- instance ToJSON Datum
type DatumId = Int

type DatumAPI = APIFor Datum DatumId

datumAPI :: Proxy DatumAPI
datumAPI = Proxy

datum1Id = 1

datum1 =
  Datum
    "週明けは全国的に不安定な天気に 急な雷雨や激しい突風に注意"
    TText
    "低気圧が日本海西部にあって東に進んでおり、寒冷前線が山陰沖から九州北部地方にのびている。この低気圧や前線に向かって暖かく湿った空気が流れ込んで大気の状態が非常に不安定となっており、西日本では雷を伴った激しい雨となっている所がある。"
    (read "2019-11-11 07:55:02" :: UTCTime)

datums = [(datum1Id, datum1)]

datumServer :: Server DatumAPI
datumServer = getEntities :<|> newEntity :<|> operations
  where
    getEntities = liftIO $ return datums
    newEntity = error "feature not implemented yet"
    operations id' = getEntity id' :<|> updateEntity id' :<|> deleteEntity id'
    getEntity id' =
      liftIO . return . head $ [x | (datumId, x) <- datums, datumId == id']
    updateEntity id' = error "feature not implemented yet"
    deleteEntity id' = error "feature not implemented yet"

app :: Application
app = serve datumAPI datumServer

deriveBoth defaultOptions ''DatumType

deriveBoth defaultOptions ''Datum
