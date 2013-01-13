{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Biegunka.Source.Darcs - functions to work with darcs repositories as sources
module Biegunka.Source.Darcs
  ( -- * Source layer
    darcs, darcs_
  ) where

import           Control.Exception     (SomeException (..), fromException,
                                        handle)
import           Control.Monad.Free    (liftF)
import           Data.Text             (pack)
import           System.Directory      (doesDirectoryExist)
import           System.Exit           (ExitCode (..))
import           System.FilePath.Posix (takeDirectory)

import           Darcs.Commands        (DarcsCommand (commandCommand))
import           Darcs.Commands.Get    (get)
import           Darcs.Commands.Pull   (pull)
import qualified Darcs.Flags           as F
import           Darcs.Utils           (withCurrentDirectory)


import           Biegunka.Language     (Command (S), Layer (Files, Source),
                                        Script)
import           Biegunka.Source.Dummy (sourceFailure)


-- | Clone repository from the given url to specified path and/or pull.
-- Also executes attached script
--
-- > darcs "https://example.com/repository" "darcs/repository" $ do
-- >   registerAt "some/not/so/long/path"
-- >   link "important.file" ".config"
--
-- * get repository from https:\/\/example.com\/repository to ${HOME}\/darcs\/repository
--
-- * link ${HOME}\/darcs\/repository to ${HOME}\/some\/not\/so\/long\/path
--
-- * link ${HOME}\/darcs\/repository\/important.file to ${HOME}\/.config
darcs ∷ String → FilePath → Script Files () → Script Source ()
darcs url path script = liftF $ S url path script (updateDarcs url) ()


-- | Clone repository from the given url to specified path
--
-- > darcs_ "https://example.com/repository" "darcs/repository"
--
-- * get repository from https:\/\/example.com\/repository to ${HOME}\/darcs\/repository
darcs_ ∷ String → FilePath → Script Source ()
darcs_ url path = darcs url path (return ())


updateDarcs ∷ String → FilePath → IO ()
updateDarcs url path = do
  exists ← doesDirectoryExist path
  handle check $ if exists
    then -- pull
      withCurrentDirectory path $ commandCommand pull [F.Quiet] [url]
    else -- get
      withCurrentDirectory parent_path $ commandCommand get [F.Quiet] [url]
 where

  parent_path = takeDirectory path

  check ∷ SomeException → IO ()
  check e =
    case fromException e ∷ Maybe ExitCode of
      Just ExitSuccess → return ()
      Just _ → f
      Nothing → f
   where
    f = sourceFailure url path $ pack $ show e
