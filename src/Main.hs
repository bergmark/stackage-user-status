{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    FlexibleContexts
  , GADTs
  , MultiParamTypeClasses
  , NoImplicitPrelude
  , OverloadedStrings
  , ScopedTypeVariables
  , ViewPatterns
  #-}
module Main where

import ClassyPrelude
import Data.String.Conversions hiding ((<>))
import Data.String.Conversions.Monomorphic
import Distribution.Package (PackageName)
import Distribution.Text (disp)
import Distribution.Version
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Stackage.BuildConstraints
import Stackage.Types
import Text.PrettyPrint (render)
import qualified Data.HashMap.Strict as H
import qualified Data.Set            as S

data Packages = Package
  { stackageMaintainer :: !Text
  , packages           :: !(Set Text)
  } deriving (Eq, Show)

main :: IO ()
main = do
  a <- getArgs
  case a of
    []     -> error "No user specified"
    user:_ -> run user

run :: ConvertibleStrings s StrictText => s -> IO ()
run (toStrictText -> user) = do
  manager <- newManager tlsManagerSettings
  putStrLn "Parsing build-constraints.yaml"
  bc <- defaultBuildConstraints manager
  putStrLn "Parsed build-constraints.yaml"
  let pns = filter (maybe False ((user `isInfixOf`) . unMaintainer) . pcMaintainer . bcPackageConstraints bc) . S.toList $ bcPackages bc
  forM_ (groupUsers . zip pns $ map (bcPackageConstraints bc) pns) $ \(m, xs) -> do
    putStrLn ""
    putStrLn $ "# Packages for " <> unMaintainer m
    printPackageInfos "Bounds"       xs (not . isAnyVersion . pcVersionRange . snd) (showF . render . disp . pcVersionRange)
    printPackageInfos "Tests"        xs ((/= ExpectSuccess) . pcTests    . snd) (showF . show . pcTests)
    printPackageInfos "Benchmarks"   xs ((/= ExpectSuccess) . pcBenches  . snd) (showF . show . pcBenches)
    printPackageInfos "Haddocks"     xs ((/= ExpectSuccess) . pcHaddocks . snd) (showF . show . pcHaddocks)
    printPackageInfos "All packages" xs (const True) (const Nothing)

printPackageInfos :: Text
                  -> [(PackageName, PackageConstraints)]
                  -> ((PackageName, PackageConstraints) -> Bool)
                  -> (PackageConstraints -> Maybe Text)
                  -> IO ()
printPackageInfos title xs p m = do
  let with = filter p xs
  unless (null with) $ do
    putStrLn $ "## " <> title
    forM_ with $ \(pn, pcs) -> do
      putStrLn $ cs pn <> fromMaybe "" (m pcs)

groupUsers :: [(PackageName, PackageConstraints)] -> [(Maintainer, [(PackageName, PackageConstraints)])]
groupUsers = map (second $ sortBy (comparing fst)) . H.toList . foldl' (\h (pn, pcs) -> maybe h (\m -> H.insertWith (<>) m [(pn, pcs)] h) $ pcMaintainer pcs) mempty

instance ConvertibleStrings PackageName  Text where convertString = unPackageName
instance ConvertibleStrings VersionRange Text where convertString = cs . render . disp

showF :: String -> Maybe StrictText
showF = Just . (": " <>) . cs
