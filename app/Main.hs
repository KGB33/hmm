import Data.List
import Data.String.Utils
import Options.Applicative
import System.Process

-- Parser Info
repo :: Parser (Maybe String)
repo =
  optional $
    strOption
      ( long "repo"
          <> short 'r'
          <> metavar "REPO"
          <> help "First part of the tmux session name."
      )

branch :: Parser (Maybe String)
branch =
  optional $
    strOption
      ( long "branch"
          <> short 'b'
          <> metavar "BRANCH"
          <> help "Second part of the tmux session name."
      )

data Options = Options
  { optRepo :: Maybe String,
    optBranch :: Maybe String
  }

opts :: Parser Options
opts = Options <$> repo <*> branch

-- Main
main :: IO ()
main = hmm =<< execParser args
  where
    args =
      info
        (opts <**> helper)
        ( fullDesc
            <> progDesc "prog Desc"
            <> header "header"
        )

-- EntryPoint
hmm :: Options -> IO ()
hmm options = do
  r <- maybe computeDefaultRepo return (optRepo options)
  b <- maybe computeDefaultBranch return (optBranch options)
  let sessionName :: String = r ++ "-" ++ b

  (_, currSessions, _) <- readProcessWithExitCode "tmux" ["ls"] []
  let cmd = case currSessions of
        s | sessionName `isInfixOf` s -> ["attach-session", "-t"]
        _ -> ["new-session", "-s"]
  callProcess "tmux" $ cmd ++ [sessionName]

-- Calcuate Branch & Repo info
computeDefaultBranch :: IO String
computeDefaultBranch = do
  b <- readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] []
  return (strip b)

-- There are two possible options:
--   * The user is in a worktree:
--        `/home/kgb33/Code/nasty/worktrees/main`
--   * The user is in a standard repo:
--        `/home/kgb33/Code/hmm/.git`
-- Because there is only zero or one element to the right of our anchor element,
-- Its easiest to revese the list before massing it to the fucntion and match 'backward'
-- anchor \/ , repo \/
-- i.e. [".git", "hmm", "Code", ...]
parseDefaultRepo :: [String] -> String
parseDefaultRepo (_ : "worktrees" : r : _) = r
parseDefaultRepo (".git" : r : _) = r
parseDefaultRepo _ = "unk"

computeDefaultRepo :: IO String
computeDefaultRepo = do
  r <- readProcess "git" ["rev-parse", "--path-format=absolute", "--git-dir"] []
  let s = strip r
  let l = split "/" s
  return $ parseDefaultRepo (reverse l)

