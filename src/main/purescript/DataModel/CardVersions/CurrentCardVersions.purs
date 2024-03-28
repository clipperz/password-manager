module DataModel.CardVersions.CurrentCardVersions where

import Data.Codec.Argonaut (JsonCodec)
import DataModel.CardVersions.Card (CardVersion(..))
import DataModel.CardVersions.CardV1 (Card_V1, cardV1Codec)

currentCardVersion :: CardVersion
currentCardVersion = CardVersion_1

currentCardCodecVersion :: JsonCodec Card_V1
currentCardCodecVersion = cardV1Codec