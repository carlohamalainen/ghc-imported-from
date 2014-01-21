{-# LANGUAGE RankNTypes #-}

module ImportedFromSpec where

import Control.Monad.Writer
import Data.List (isSuffixOf, isInfixOf, isPrefixOf)
import GhcImportedFrom
import System.FilePath()
import Test.Hspec

import Dir

isRight :: forall a b. Either a b -> Bool
isRight = either (const False) (const True)

spec :: Spec
spec = do
    describe "checkImportedFrom" $ do
        it "can look up Maybe" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "Maybe"         11 11 (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up Just" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "Just"          12 7  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up Just" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "Just"          16 10 (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up String" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "String"        20 14 (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up Int" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "Int"           22 23 (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up DL.length" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "DL.length"     23 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up print" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "print"         25 8  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up DM.fromList" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "DM.fromList"   27 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up Safe.headMay" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Muddle.hs" "Muddle" "Safe.headMay"  29 6  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up map" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Hiding.hs" "Hiding" "map"           12 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up head" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "Hiding.hs" "Hiding" "head"          12 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up when" $ do
            withDirectory_ "test/data" $ do
                (res, _) <- runWriterT $ guessHaddockUrl "When.hs"   "When"   "when"          15 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight
