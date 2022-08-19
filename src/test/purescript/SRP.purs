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
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect.Aff (Aff)
import Test.Spec (describe, it, SpecT)
import Test.Spec.Assertions (shouldEqual)
import SRP as SRP
import TestUtilities (makeTestableOnBrowser, failOnBrowser)

srpSpec :: SpecT Aff Unit Identity Unit
srpSpec =
  describe "SRP" do
    let srpConfiguration = {
      group: SRP.group1024,
      k: (fromMaybe SRP.bigInt0 (toBigInt $ hex "7556AA045AEF2CDD07ABAF0F665C3E818913186F")),
      hash: SRP.hashFuncSHA256,
      kdf: SRP.getPrepareX SRP.hashFuncSHA256
    }

    let testGroup1024 = "uses group1024"
    it testGroup1024 do
      let n = "167609434410335061345139523764350090260135525329813904557420930309800865859473551531551523800013916573891864789934747039010546328480848979516637673776605610374669426214776197828492691384519453218253702788022233205683635831626913357154941914129985489522629902540768368409482248290641036967659389658897350067939"
      makeTestableOnBrowser (testGroup1024 <> " 1") (toString SRP.group1024.nn) shouldEqual n
      makeTestableOnBrowser (testGroup1024 <> " 2") SRP.group1024.g shouldEqual (fromInt 2)
      
    let testk = "uses k"
    it testk do
      let k = "669884594844073113358786362162819048475760728175"
      makeTestableOnBrowser testk (toString srpConfiguration.k) shouldEqual k

    let computesA = "computes A"
    it computesA do
      let a  = fromMaybe SRP.bigInt0 (toBigInt $ hex "60975527 035CF2AD 1989806F 0407210B C81EDC04 E2762A56 AFD529DD DA2D4393")
      let aa = fromMaybe SRP.bigInt0 (toBigInt $ hex $ "61D5E490 F6F1B795 47B0704C 436F523D D0E560F0 C64115BB 72557EC4"
                                              <> "4352E890 3211C046 92272D8B 2D1A5358 A2CF1B6E 0BFCF99F 921530EC"
                                              <> "8E393561 79EAE45E 42BA92AE ACED8251 71E1E8B9 AF6D9C03 E1327F44"
                                              <> "BE087EF0 6530E69F 66615261 EEF54073 CA11CF58 58F0EDFD FE15EFEA"
                                              <> "B349EF5D 76988A36 72FAC47B 0769447B")
      makeTestableOnBrowser computesA (SRP.computeA srpConfiguration a) shouldEqual aa
    
    let computesB = "computes B"
    it computesB do
      let b = fromMaybe SRP.bigInt0 (toBigInt $ hex "E487CB59 D31AC550 471E81F0 0F6928E0 1DDA08E9 74A004F4 9E61F5D1 05284D20")
      let v = fromMaybe SRP.bigInt0 (toBigInt $ hex $ "7E273DE8 696FFC4F 4E337D05 B4B375BE B0DDE156 9E8FA00A 9886D812"
                                              <> "9BADA1F1 822223CA 1A605B53 0E379BA4 729FDC59 F105B478 7E5186F5"
                                              <> "C671085A 1447B52A 48CF1970 B4FB6F84 00BBF4CE BFBB1681 52E08AB5"
                                              <> "EA53D15C 1AFF87B2 B9DA6E04 E058AD51 CC72BFC9 033B564E 26480D78"
                                              <> "E955A5E2 9E7AB245 DB2BE315 E2099AFB")
      let bb = fromMaybe SRP.bigInt0 (toBigInt $ hex $ "BD0C6151 2C692C0C B6D041FA 01BB152D 4916A1E7 7AF46AE1 05393011"
                                              <> "BAF38964 DC46A067 0DD125B9 5A981652 236F99D9 B681CBF8 7837EC99"
                                              <> "6C6DA044 53728610 D0C6DDB5 8B318885 D7D82C7F 8DEB75CE 7BD4FBAA"
                                              <> "37089E6F 9C6059F3 88838E7A 00030B33 1EB76840 910440B1 B27AAEAE"
                                              <> "EB4012B7 D7665238 A8E3FB00 4B117B58")
      makeTestableOnBrowser computesB (SRP.computeB srpConfiguration b v) shouldEqual bb

    let computesSClient = "computes client premaster secret"
    it computesSClient do
      let ss = fromMaybe SRP.bigInt0 (toBigInt $ hex $ "B0DC82BA BCF30674 AE450C02 87745E79 90A3381F 63B387AA F271A10D"
                                              <> "233861E3 59B48220 F7C4693C 9AE12B0A 6F67809F 0876E2D0 13800D6C"
                                              <> "41BB59B6 D5979B5C 00A172B4 A2A5903A 0BDCAF8A 709585EB 2AFAFA8F"
                                              <> "3499B200 210DCC1F 10EB3394 3CD67FC8 8A2F39A4 BE5BEC4E C0A3212D"
                                              <> "C346D7E4 74B29EDE 8A469FFE CA686E5A")
      let a = fromMaybe SRP.bigInt0 (toBigInt $ hex "60975527 035CF2AD 1989806F 0407210B C81EDC04 E2762A56 AFD529DD DA2D4393")
      let bb = fromMaybe SRP.bigInt0 (toBigInt $ hex $ "BD0C6151 2C692C0C B6D041FA 01BB152D 4916A1E7 7AF46AE1 05393011"
                                              <> "BAF38964 DC46A067 0DD125B9 5A981652 236F99D9 B681CBF8 7837EC99"
                                              <> "6C6DA044 53728610 D0C6DDB5 8B318885 D7D82C7F 8DEB75CE 7BD4FBAA"
                                              <> "37089E6F 9C6059F3 88838E7A 00030B33 1EB76840 910440B1 B27AAEAE"
                                              <> "EB4012B7 D7665238 A8E3FB00 4B117B58")
      let x = fromMaybe SRP.bigInt0 (toBigInt $ hex "94B7555A ABE9127C C58CCF49 93DB6CF8 4D16C124")
      let u = fromMaybe SRP.bigInt0 (toBigInt $ hex "CE38B959 3487DA98 554ED47D 70A7AE5F 462EF019")
      makeTestableOnBrowser computesSClient (SRP.computeSClient srpConfiguration bb x a u) shouldEqual ss
      
    
    let computesSServer = "computes server premaster secret"
    it computesSServer do
      let ss = fromMaybe SRP.bigInt0 (toBigInt $ hex $ "B0DC82BA BCF30674 AE450C02 87745E79 90A3381F 63B387AA F271A10D"
                                              <> "233861E3 59B48220 F7C4693C 9AE12B0A 6F67809F 0876E2D0 13800D6C"
                                              <> "41BB59B6 D5979B5C 00A172B4 A2A5903A 0BDCAF8A 709585EB 2AFAFA8F"
                                              <> "3499B200 210DCC1F 10EB3394 3CD67FC8 8A2F39A4 BE5BEC4E C0A3212D"
                                              <> "C346D7E4 74B29EDE 8A469FFE CA686E5A")
      let aa = fromMaybe SRP.bigInt0 (toBigInt $ hex $ "61D5E490 F6F1B795 47B0704C 436F523D D0E560F0 C64115BB 72557EC4"
                                              <> "4352E890 3211C046 92272D8B 2D1A5358 A2CF1B6E 0BFCF99F 921530EC"
                                              <> "8E393561 79EAE45E 42BA92AE ACED8251 71E1E8B9 AF6D9C03 E1327F44"
                                              <> "BE087EF0 6530E69F 66615261 EEF54073 CA11CF58 58F0EDFD FE15EFEA"
                                              <> "B349EF5D 76988A36 72FAC47B 0769447B")
      let v = fromMaybe SRP.bigInt0 (toBigInt $ hex $ "7E273DE8 696FFC4F 4E337D05 B4B375BE B0DDE156 9E8FA00A 9886D812"
                                              <> "9BADA1F1 822223CA 1A605B53 0E379BA4 729FDC59 F105B478 7E5186F5"
                                              <> "C671085A 1447B52A 48CF1970 B4FB6F84 00BBF4CE BFBB1681 52E08AB5"
                                              <> "EA53D15C 1AFF87B2 B9DA6E04 E058AD51 CC72BFC9 033B564E 26480D78"
                                              <> "E955A5E2 9E7AB245 DB2BE315 E2099AFB")
      let b = fromMaybe SRP.bigInt0 (toBigInt $ hex "E487CB59 D31AC550 471E81F0 0F6928E0 1DDA08E9 74A004F4 9E61F5D1 05284D20")
      let u = fromMaybe SRP.bigInt0 (toBigInt $ hex "CE38B959 3487DA98 554ED47D 70A7AE5F 462EF019")
      makeTestableOnBrowser computesSServer (SRP.computeSServer srpConfiguration aa v b u) shouldEqual ss

    let srpCorrectness = "computes the same secret on client and server"
    it srpCorrectness do
      let username = "joe"
      let password = "clipperz"
      p <- SRP.prepareP srpConfiguration username password
      s <- SRP.randomArrayBuffer 32
      result :: Either SRP.SRPError (Tuple BigInt BigInt) <- runExceptT $ do
        vHex :: HexString <- (ExceptT $ SRP.prepareV srpConfiguration s p)
        v    :: BigInt <- except $ note (SRP.SRPError "Cannot covert v from HexString to BigInt") (toBigInt vHex)
        (Tuple a aa) <- ExceptT $ SRP.prepareA srpConfiguration
        (Tuple b bb) <- ExceptT $ SRP.prepareB srpConfiguration v
        xAb <- ExceptT $ Right <$> srpConfiguration.kdf s p
        x   <- except $ note (SRP.SRPError "Cannot convert x from HexString to BigInt") (toBigInt (fromArrayBuffer xAb))
        sClient <- ExceptT $ SRP.prepareSClient srpConfiguration aa bb x a
        sServer <- ExceptT $ SRP.prepareSServer srpConfiguration aa bb v b
        pure $ Tuple sClient sServer
      case result of
        Left err -> failOnBrowser srpCorrectness (show err)
        Right (Tuple sClient sServer) -> makeTestableOnBrowser srpCorrectness sClient shouldEqual sServer
