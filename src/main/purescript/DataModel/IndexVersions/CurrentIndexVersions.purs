module DataModel.IndexVersions.CurrentIndexVersions where

import Data.Codec.Argonaut (JsonCodec)
import DataModel.IndexVersions.Index (IndexVersion(..))
import DataModel.IndexVersions.IndexV1 (Index_V1, indexV1Codec)

currentIndexVersion :: IndexVersion
currentIndexVersion = IndexVersion_1

currentIndexCodecVersion :: JsonCodec Index_V1
currentIndexCodecVersion = indexV1Codec
