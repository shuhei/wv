module VocabularySpec where

import Test.Hspec
import Vocabulary
import Data.HashMap.Strict ((!), empty)

sampleSentences :: [Sentence]
sampleSentences = map words $
  [
    "haskell is a standardized general-purpose purely functional language with non-strict semantics and strong static typing"
  , "it is named after logician haskell curry"
  , "following the release of miranda by research software ltd in 1986 interest in lazy functional languages grew by 1987 more than a dozen non-strict purely functional programming languages existed"
  , "of these miranda was the most widely used but was proprietary software"
  , "at the conference on functional programming languages and computer architecture fpca '87 in portland oregon a meeting was held during which participants formed a strong consensus that a committee should be formed to define an open standard for such languages"
  , "the committee's purpose was to consolidate the existing functional languages into a common one that would serve as a basis for future research in functional-language design"
  ]

spec :: Spec
spec = do
  describe "buildModel" $ do
    it "builds a model" $ do
      (voc, i2w, n) <- return $ buildModel sampleSentences
      n `shouldBe` 89
      voc ! "haskell" `shouldBe` (0, 2)
      voc ! "functional" `shouldBe` (6, 5)
      i2w ! 0 `shouldBe` "haskell"
      i2w ! 6 `shouldBe` "functional"
