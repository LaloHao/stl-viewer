import STL.Parser
import System.Environment

main :: IO ()
main = do
  [file] <- getArgs
  p <- stlParseFromFile file
  case p of
    Left  error  -> putStrLn $ show error
    Right (Solid name facets) -> do
     putStrLn $ "Solid name: " ++ name
     putStrLn $ "Facets: " ++ (show $ length facets)
     putStrLn $ unlines $ (show <$> facets)
  return ()
