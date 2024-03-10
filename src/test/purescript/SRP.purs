module Test.SRP where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except)
import Data.BigInt (BigInt, toString, fromInt)
import Data.Either (Either(..), note)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, hex, toBigInt, fromArrayBuffer)
import Data.Identity (Identity)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.SRPVersions.SRP(group1024, SRPConf, bigInt0, hashFuncSHA256, hashFuncSHA1, concatKDF, SRPError(..))
import Functions.SRP as SRP
import Effect.Aff (Aff)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import Test.QuickCheck (Result(..), (===))
import Test.QuickCheck.Gen (Gen)
import TestClasses (AsciiString)
import TestUtilities (makeTestableOnBrowser, quickCheckAffInBrowser)

srpSpec :: SpecT Aff Unit Identity Unit
srpSpec =
  describe "SRP" do
    let samples = 50

    let srpConfiguration = {
      group: group1024,
      k: (fromMaybe bigInt0 (toBigInt $ hex "7556AA045AEF2CDD07ABAF0F665C3E818913186F")),
      hash: hashFuncSHA256,
      kdf: concatKDF
    }

    let testGroup1024 = "uses group1024"
    let srpConfigurationForRFC = {
      group: {
        nn: rfcTestVector.nn
      , g: rfcTestVector.g
      },
      k: rfcTestVector.k,
      hash: hashFuncSHA1,
      kdf: concatKDF
    }

    it testGroup1024 do
      let n = "167609434410335061345139523764350090260135525329813904557420930309800865859473551531551523800013916573891864789934747039010546328480848979516637673776605610374669426214776197828492691384519453218253702788022233205683635831626913357154941914129985489522629902540768368409482248290641036967659389658897350067939"
      makeTestableOnBrowser (testGroup1024 <> " 1") (toString group1024.nn) shouldEqual n
      makeTestableOnBrowser (testGroup1024 <> " 2") group1024.g shouldEqual (fromInt 2)
      
    let testk = "uses k"
    it testk do
      let k = "669884594844073113358786362162819048475760728175"
      makeTestableOnBrowser testk (toString srpConfiguration.k) shouldEqual k

    testSrpBasicFunctions srpConfigurationForRFC "RFC Test Vector" rfcTestVector 
    
    let srpCorrectness = "computes the same secret on client and server"
    it srpCorrectness do
      quickCheckAffInBrowser srpCorrectness samples (sameSecretProp srpConfiguration)

sameSecretProp :: SRPConf -> AsciiString -> AsciiString -> Gen (Aff Result)
sameSecretProp srpConfiguration username' password' = pure $ do
  let username = unwrap username'
  let password = unwrap password'
  p <- SRP.prepareP srpConfiguration username password
  s <- SRP.randomArrayBuffer 32
  result :: Either SRPError (Tuple BigInt BigInt) <- runExceptT $ do
    vHex :: HexString <- (ExceptT $ SRP.prepareV srpConfiguration s p)
    v    :: BigInt <- except $ note (SRPError "Cannot covert v from HexString to BigInt") (toBigInt vHex)
    (Tuple a aa) <- ExceptT $ SRP.prepareA srpConfiguration
    (Tuple b bb) <- ExceptT $ SRP.prepareB srpConfiguration v
    xAb <- ExceptT $ Right <$> srpConfiguration.kdf srpConfiguration.hash s p
    x   <- except $ note (SRPError "Cannot convert x from HexString to BigInt") (toBigInt (fromArrayBuffer xAb))
    sClient <- ExceptT $ SRP.prepareSClient srpConfiguration aa bb x a
    sServer <- ExceptT $ SRP.prepareSServer srpConfiguration aa bb v b
    pure $ Tuple sClient sServer
  pure $ case result of
    Left err -> Failed (show err)
    Right (Tuple sClient sServer) -> sClient === sServer
-----

type TestVector = { c :: String -- I
                  , p :: String -- P
                  , s :: BigInt
                  , nn :: BigInt
                  , g :: BigInt
                  , k :: BigInt
                  , x :: BigInt
                  , v :: BigInt
                  , a :: BigInt
                  , b :: BigInt
                  , aa :: BigInt
                  , bb :: BigInt
                  , u :: BigInt
                  , secret :: BigInt
                  }

rfcTestVector :: TestVector
rfcTestVector = { c: "alice"
                , p: "password123"
                , s: fromMaybe bigInt0 $ toBigInt $ hex $ "BEB25379 D1A8581E B5A72767 3A2441EE"
                , nn: fromMaybe bigInt0 $ toBigInt $ hex $ "EEAF0AB9 ADB38DD6 9C33F80A FA8FC5E8 60726187 75FF3C0B 9EA2314C"
                                          <> "9C256576 D674DF74 96EA81D3 383B4813 D692C6E0 E0D5D8E2 50B98BE4"
                                          <> "8E495C1D 6089DAD1 5DC7D7B4 6154D6B6 CE8EF4AD 69B15D49 82559B29"
                                          <> "7BCF1885 C529F566 660E57EC 68EDBC3C 05726CC0 2FD4CBF4 976EAA9A"
                                          <> "FD5138FE 8376435B 9FC61D2F C0EB06E3"
                , g: fromMaybe bigInt0 $ toBigInt $ hex $ "2"
                , k: fromMaybe bigInt0 $ toBigInt $ hex $ "7556AA04 5AEF2CDD 07ABAF0F 665C3E81 8913186F"
                , x: fromMaybe bigInt0 $ toBigInt $ hex $ "94B7555A ABE9127C C58CCF49 93DB6CF8 4D16C124"
                , v: fromMaybe bigInt0 $ toBigInt $ hex $ "7E273DE8 696FFC4F 4E337D05 B4B375BE B0DDE156 9E8FA00A 9886D812"
                                              <> "9BADA1F1 822223CA 1A605B53 0E379BA4 729FDC59 F105B478 7E5186F5"
                                              <> "C671085A 1447B52A 48CF1970 B4FB6F84 00BBF4CE BFBB1681 52E08AB5"
                                              <> "EA53D15C 1AFF87B2 B9DA6E04 E058AD51 CC72BFC9 033B564E 26480D78"
                                              <> "E955A5E2 9E7AB245 DB2BE315 E2099AFB"
                , a: fromMaybe bigInt0 $ toBigInt $ hex $ "60975527 035CF2AD 1989806F 0407210B C81EDC04 E2762A56 AFD529DD DA2D4393"
                , b: fromMaybe bigInt0 $ toBigInt $ hex $ "E487CB59 D31AC550 471E81F0 0F6928E0 1DDA08E9 74A004F4 9E61F5D1 05284D20"
                , aa: fromMaybe bigInt0 $ toBigInt $ hex $ "61D5E490 F6F1B795 47B0704C 436F523D D0E560F0 C64115BB 72557EC4"
                                              <> "4352E890 3211C046 92272D8B 2D1A5358 A2CF1B6E 0BFCF99F 921530EC"
                                              <> "8E393561 79EAE45E 42BA92AE ACED8251 71E1E8B9 AF6D9C03 E1327F44"
                                              <> "BE087EF0 6530E69F 66615261 EEF54073 CA11CF58 58F0EDFD FE15EFEA"
                                              <> "B349EF5D 76988A36 72FAC47B 0769447B"
                , bb: fromMaybe bigInt0 $ toBigInt $ hex $ "BD0C6151 2C692C0C B6D041FA 01BB152D 4916A1E7 7AF46AE1 05393011"
                                            <> "BAF38964 DC46A067 0DD125B9 5A981652 236F99D9 B681CBF8 7837EC99"
                                            <> "6C6DA044 53728610 D0C6DDB5 8B318885 D7D82C7F 8DEB75CE 7BD4FBAA"
                                            <> "37089E6F 9C6059F3 88838E7A 00030B33 1EB76840 910440B1 B27AAEAE"
                                            <> "EB4012B7 D7665238 A8E3FB00 4B117B58"
                , u: fromMaybe bigInt0 $ toBigInt $ hex $ "CE38B959 3487DA98 554ED47D 70A7AE5F 462EF019"
                , secret: fromMaybe bigInt0 $ toBigInt $ hex $ "B0DC82BA BCF30674 AE450C02 87745E79 90A3381F 63B387AA F271A10D"
                                              <> "233861E3 59B48220 F7C4693C 9AE12B0A 6F67809F 0876E2D0 13800D6C"
                                              <> "41BB59B6 D5979B5C 00A172B4 A2A5903A 0BDCAF8A 709585EB 2AFAFA8F"
                                              <> "3499B200 210DCC1F 10EB3394 3CD67FC8 8A2F39A4 BE5BEC4E C0A3212D"
                                              <> "C346D7E4 74B29EDE 8A469FFE CA686E5A"
                }

testSrpBasicFunctions :: SRPConf -> String -> TestVector -> SpecT Aff Unit Identity Unit
testSrpBasicFunctions srpConfiguration vectorName tv = do
  let computesA = "computes A - " <> vectorName
  it computesA do
    makeTestableOnBrowser computesA (SRP.computeA srpConfiguration tv.a) shouldEqual tv.aa

  let computesB = "computes B - " <> vectorName
  it computesB do
    makeTestableOnBrowser computesB (SRP.computeB srpConfiguration tv.b tv.v) shouldEqual tv.bb

  let computesSClient = "computes client premaster secret - " <> vectorName
  it computesSClient do
    makeTestableOnBrowser computesSClient (SRP.computeSClient srpConfiguration tv.bb tv.x tv.a tv.u) shouldEqual tv.secret

  let computesSServer = "computes server premaster secret - " <> vectorName
  it computesSServer do
    makeTestableOnBrowser computesSServer (SRP.computeSServer srpConfiguration tv.aa tv.v tv.b tv.u) shouldEqual tv.secret
