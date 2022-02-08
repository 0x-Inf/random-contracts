{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (unpack)
import Data.Proxy             (Proxy (..))
import Data.Text              (pack)
import Data.UUID
import Network.HTTP.Req
import Text.Regex.TDFA

main :: IO ()
main = do
    uuid <- read <$> readFile "oracle.cid"
    putStrLn $ "oracle contract instance id: " ++ show uuid
    go uuid Nothing
  where
    go :: UUID -> Maybe Integer -> IO a
    go uuid m = do
        x <- getExchangeRate
        let y = Just x
        when (m /= y) $
            updateOracle uuid x
        threadDelay 5_000_000
        go uuid y

updateOracle :: UUID -> Integer -> IO ()
updateOracle uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "update")
        (ReqBodyJson x)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "updated oracle to " ++ show x
        else "error updating oracle"

getExchangeRate :: IO Integer
getExchangeRate = runReq defaultHttpConfig $ do
    -- let payload =
    --     object
    --       [ "Accepts" .= "application/json",
    --         "X-CMC_PRO_API_KEY" .= myToken
    --       ]
    v <- req
        GET
        (https "pro-api.coinmarketcap.com/v1" /: "cryptocurrency" /: "info") -- https://pro-api.coinmarketcap.com
        NoReqBody
        bsResponse $ 
        header "Accepts" "application/json" <>
        "X-CMC_PRO_API_KEY" `header` "14785900-0f5c-4f77-93dc-08fdd43d3a2e"  
    let priceRegex      = "priceValue___11gHJ \">\\$([\\.0-9]*)" :: ByteString
        (_, _, _, [bs]) = responseBody v =~ priceRegex :: (ByteString, ByteString, ByteString, [ByteString])
        d               = read $ unpack bs :: Double
        x               = round $ 1_000_000 * d
    liftIO $ putStrLn $ "queried exchange rate: " ++ show d
    return x
    
    where 
        myToken :: ByteString 
        myToken = "14785900-0f5c-4f77-93dc-08fdd43d3a2e"
