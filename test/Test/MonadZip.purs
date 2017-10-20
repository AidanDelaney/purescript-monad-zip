module Test.MonadZip where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects, run)

-- Component under test
import Control.Monad.Zip

assertionSpec :: ∀ e. Spec (RunnerEffects e) Unit
assertionSpec =
     describe "purescript-monoad-sum" do
       describe "Simple Tests" do
         it "Zips (Just 1) Nothing" $
           (mzip (Nothing :: Maybe Int) (Nothing :: Maybe Int)) `shouldEqual` Nothing
         it "Zips (Just 1) (Just 2)" $
           Just (Tuple 1 2) `shouldEqual` mzip (Just 1) (Just 2)
       describe "Laws Tests" do
         it "Preserve Naturility" $
           liftM1 (f *** g) (mzip ma mb) `shouldEqual` mzip (liftM1 f ma) (liftM1 g mb)
         it "Information Preservation" $
           munzip (mzip ma mb) `shouldEqual` Tuple ma mb
       where
         ma = (Just 1)
         mb = (Just 2)
         f = (\x -> x*x)
         g = (\x -> x+1)
