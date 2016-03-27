{-# LANGUAGE RankNTypes #-}

module ImportedFromSpec where

import Language.Haskell.GhcImportedFrom
import System.FilePath()
import Test.Hspec

import Control.Exception as E
import Data.List (isPrefixOf)
import System.Directory
import System.FilePath (addTrailingPathSeparator)

-------------------------------------------------------------------------------
-- withDirectory_, withDirectory, and toRelativeDir are copied
-- from ghc-mod.
withDirectory_ :: FilePath -> IO a -> IO a
withDirectory_ dir action = bracket getCurrentDirectory
                                    setCurrentDirectory
                                    (\_ -> setCurrentDirectory dir >> action)

withDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withDirectory dir action = bracket getCurrentDirectory
                                   setCurrentDirectory
                                   (\d -> setCurrentDirectory dir >> action d)

toRelativeDir :: FilePath -> FilePath -> FilePath
toRelativeDir dir file
  | dir' `isPrefixOf` file = drop len file
  | otherwise              = file
  where
    dir' = addTrailingPathSeparator dir
    len = length dir'

isRight :: forall a b. Either a b -> Bool
isRight = either (const False) (const True)
-------------------------------------------------------------------------------

-- Instead of shouldSatisfy isRight, these should check for the right module/package
-- name turning up in the results.

spec :: Spec
spec = do
    describe "checkImportedFrom" $ do
        it "can look up Maybe" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "Maybe"         11 11 (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up Just" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "Just"          12 7  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up Just" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "Just"          16 10 (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up String" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "String"        20 14 (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up Int" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "Int"           22 23 (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up DL.length" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "DL.length"     23 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up print" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "print"         25 8  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up DM.fromList" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "DM.fromList"   27 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up Safe.headMay" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Muddle.hs" "Muddle" "Safe.headMay"  29 6  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up map" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Hiding.hs" "Hiding" "map"           14 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up head" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "Hiding.hs" "Hiding" "head"          16 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

        it "can look up when" $ do
            withDirectory_ "test/data" $ do
                res <- guessHaddockUrl "When.hs"   "When"   "when"          15 5  (GhcOptions []) (GhcPkgOptions [])
                res `shouldSatisfy` isRight

