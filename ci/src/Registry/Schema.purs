module Registry.Schema where

import Registry.Prelude

import Data.Argonaut (jsonEmptyObject, (~>), (~>?), (:=), (:=?), (.:), (.:?), (.!=))
import Data.Argonaut as Json
import Data.Generic.Rep as Generic
import Foreign.Object as Object
import Foreign.SPDX (License)
import Foreign.SemVer (SemVer, Range)
import Foreign.SemVer as SemVer
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

-- | PureScript encoding of ../v1/Manifest.dhall
newtype Manifest = Manifest
  { name :: PackageName
  , version :: SemVer
  , license :: License
  , repository :: Repo
  , targets :: Object Target
  , description :: Maybe String
  }

derive instance Eq Manifest
derive instance Newtype Manifest _
derive newtype instance Json.DecodeJson Manifest
instance Json.EncodeJson Manifest where
  encodeJson (Manifest { name, version, license, repository, targets, description }) = "description" :=? description
    ~>? "license" := license
    ~> "name" := name
    ~> "repository" := repository
    ~> "targets" := targets
    ~> "version" := version
    ~> jsonEmptyObject

type Target =
  { dependencies :: Object Range
  , sources :: Array String
  }

type RepoData d =
  { subdir :: Maybe String
  | d
  }

type GitHubData = RepoData
  ( owner :: String
  , repo :: String
  )

type GitData = RepoData (url :: String)

data Repo
  = Git GitData
  | GitHub GitHubData

derive instance eqRepo :: Eq Repo

derive instance genericRepo :: Generic.Generic Repo _

instance showRepo :: Show Repo where
  show = genericShow

-- | We encode it this way so that json-to-dhall can read it
instance repoEncodeJson :: Json.EncodeJson Repo where
  encodeJson = case _ of
    Git { subdir, url } ->
      "url" := url
        ~> "subdir" :=? subdir
        ~>? jsonEmptyObject
    GitHub { repo, owner, subdir } ->
      "githubRepo" := repo
        ~> "githubOwner" := owner
        ~> "subdir" :=? subdir
        ~>? jsonEmptyObject

instance repoDecodeJson :: Json.DecodeJson Repo where
  decodeJson json = do
    obj <- Json.decodeJson json
    subdir <- obj .:? "subdir" .!= mempty
    let
      parseGitHub = do
        owner <- obj .: "githubOwner"
        repo <- obj .: "githubRepo"
        pure $ GitHub { owner, repo, subdir }
    let
      parseGit = do
        url <- obj .: "url"
        pure $ Git { url, subdir }
    parseGitHub <|> parseGit

-- | PureScript encoding of ../v1/Operation.dhall
data Operation
  = Addition AdditionData
  | Update UpdateData
  | Unpublish UnpublishData

derive instance eqOperation :: Eq Operation

derive instance genericOperation :: Generic.Generic Operation _

instance showOperation :: Show Operation where
  show = case _ of
    Addition inner -> "Addition (" <> show (showWithPackage inner) <> ")"
    Update inner -> "Update (" <> show (showWithPackage inner) <> ")"
    Unpublish inner -> "Unpublish (" <> show (showWithPackage inner) <> ")"
    where
    showWithPackage :: forall r. { packageName :: PackageName | r } -> { packageName :: String | r }
    showWithPackage inner =
      inner { packageName = "PackageName (" <> PackageName.print inner.packageName <> ")" }

instance operationDecodeJson :: Json.DecodeJson Operation where
  decodeJson json = do
    o <- Json.decodeJson json
    packageName <- o .: "packageName"
    let
      parseAddition = do
        addToPackageSet <- o .: "addToPackageSet"
        fromBower <- o .: "fromBower"
        newPackageLocation <- o .: "newPackageLocation"
        newRef <- o .: "newRef"
        pure $ Addition { newRef, packageName, addToPackageSet, fromBower, newPackageLocation }
    let
      parseUpdate = do
        fromBower <- o .: "fromBower"
        updateRef <- o .: "updateRef"
        pure $ Update { packageName, fromBower, updateRef }
    let
      parseUnpublish = do
        unpublishVersion <- o .: "unpublishVersion"
        unpublishReason <- o .: "unpublishReason"
        pure $ Unpublish { packageName, unpublishVersion, unpublishReason }
    parseAddition <|> parseUpdate <|> parseUnpublish

type AdditionData =
  { addToPackageSet :: Boolean
  , fromBower :: Boolean
  , newPackageLocation :: Repo
  , newRef :: String
  , packageName :: PackageName
  }

type UpdateData =
  { packageName :: PackageName
  , fromBower :: Boolean
  , updateRef :: String
  }

type UnpublishData =
  { packageName :: PackageName
  , unpublishVersion :: SemVer
  , unpublishReason :: String
  }

type Metadata =
  { location :: Repo
  , releases :: Object VersionMetadata
  , unpublished :: Object String
  }

type VersionMetadata =
  { ref :: String
  , hash :: String
  }

mkNewMetadata :: Repo -> Metadata
mkNewMetadata location = { location, releases: mempty, unpublished: mempty }

addVersionToMetadata :: SemVer -> VersionMetadata -> Metadata -> Metadata
addVersionToMetadata version versionMeta metadata =
  metadata { releases = Object.insert (SemVer.version version) versionMeta metadata.releases }

isVersionInMetadata :: SemVer -> Metadata -> Boolean
isVersionInMetadata version metadata = versionPublished || versionUnpublished
  where
  versionStr = SemVer.version version
  versionPublished = isJust $ Object.lookup versionStr metadata.releases
  versionUnpublished = isJust $ Object.lookup versionStr metadata.unpublished
