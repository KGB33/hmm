import Options.Applicative
import Data.String.Utils
import System.Process

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

computeDefaultBranch :: IO String
computeDefaultBranch = do
    b <- readProcess "hostname" ["--short"] []
    return (strip b)

computeDefaultRepo :: IO String
computeDefaultRepo = do
    r <- readProcess "hostname" ["--long"] []
    return (strip r)

hmm :: Options -> IO ()
hmm options = do
  r <- maybe computeDefaultRepo return (optRepo options)
  b <- maybe computeDefaultBranch return (optBranch options)
  putStrLn $ "Debug: Repo: " ++ r ++ " Branch: " ++ b

-- hmm (Options Nothing Nothing) = putStrLn "Debug: no args provided"
-- hmm (Options (Just r) Nothing) = putStrLn $ "Debug: repo: " ++ r ++ " Branch not provided"
-- hmm (Options Nothing (Just b)) = putStrLn $ "Debug: repo not provided, Branch: " ++ b
-- hmm (Options (Just r) (Just b)) = putStrLn $ "Debug: Repo: " ++ r ++ " Branch: " ++ b
