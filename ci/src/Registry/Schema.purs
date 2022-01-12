module Registry.Schema where

import Registry.Prelude

import Data.Generic.Rep as Generic
import Foreign.Object as Object
import Foreign.SPDX (License)
import Foreign.SemVer (SemVer, Range)
import Foreign.SemVer as SemVer
import Registry.Json (class RegistryJson, (.:), (.?=), (:=))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

-- | PureScript encoding of ../v1/Manifest.dhall
newtype Manifest = Manifest
  { name :: PackageName
  , version :: SemVer
  , license :: License
  , repository :: Repo
  , targets :: Map String Target
  , description :: Maybe String
  }

derive instance Eq Manifest
derive instance Newtype Manifest _

instance RegistryJson Manifest where
  encode (Manifest fields) = Json.encodeObject do
    "name" := fields.name
    "version" := fields.version
    "license" := fields.license
    "description" := fields.description
    "repository" := fields.repository
    "targets" := fields.targets

  decode =
    map Manifest <<< Json.decode

newtype Target = Target
  { dependencies :: Map String Range
  , sources :: Array String
  }

derive instance Newtype Target _
derive newtype instance Eq Target
derive newtype instance Show Target

instance RegistryJson Target where
  encode (Target { dependencies, sources }) = Json.encodeObject do
    "sources" := sources
    "dependencies" := dependencies

  decode = map Target <<< Json.decode

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
instance RegistryJson Repo where
  encode = Json.encodeObject <<< case _ of
    Git { url, subdir } -> do
      "url" := url
      "subdir" := subdir
    GitHub { repo, owner, subdir } -> do
      "githubOwner" := owner
      "githubRepo" := repo
      "subdir" := subdir

  decode json = do
    obj <- Json.decode json
    subdir <- obj .?= "subdir"

    let
      parseGitHub = do
        owner <- obj .: "githubOwner"
        repo <- obj .: "githubRepo"
        pure $ GitHub { owner, repo, subdir }
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

instance Show Operation where
  show = case _ of
    Addition inner -> "Addition (" <> show (showWithPackage inner) <> ")"
    Update inner -> "Update (" <> show (showWithPackage inner) <> ")"
    Unpublish inner -> "Unpublish (" <> show (showWithPackage inner) <> ")"
    where
    showWithPackage :: forall r. { packageName :: PackageName | r } -> { packageName :: String | r }
    showWithPackage inner =
      inner { packageName = "PackageName (" <> PackageName.print inner.packageName <> ")" }

instance RegistryJson Operation where
  encode = case _ of
    Addition additionData -> Json.encode additionData
    Update updateData -> Json.encode updateData
    Unpublish unpublishData -> Json.encode unpublishData

  decode json = do
    obj <- Json.decode json
    packageName <- obj .: "packageName"

    let
      parseAddition = do
        addToPackageSet <- obj .: "addToPackageSet"
        fromBower <- obj .: "fromBower"
        newPackageLocation <- obj .: "newPackageLocation"
        newRef <- obj .: "newRef"
        pure $ Addition { newRef, packageName, addToPackageSet, fromBower, newPackageLocation }

      parseUpdate = do
        fromBower <- obj .: "fromBower"
        updateRef <- obj .: "updateRef"
        pure $ Update { packageName, fromBower, updateRef }

      parseUnpublish = do
        unpublishVersion <- obj .: "unpublishVersion"
        unpublishReason <- obj .: "unpublishReason"
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
