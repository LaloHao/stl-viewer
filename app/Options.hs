{-# LANGUAGE TemplateHaskell #-}
module Options where
import System.Exit
import Data.Maybe
import System.Console.GetOpt
import Control.Lens hiding ((<~), (<|))
import Text.Read
import GHC.Int

data Options = Options
  { _optScale   :: Float
  , _optRotateX :: Float
  , _optRotateY :: Float
  , _optRotateZ :: Float
  , _optWidth :: Int32
  , _optHeight :: Int32
  , _optOutputFormat :: Maybe String
  , _optOutputFolder :: Maybe FilePath
  , _optOutputFrames :: Maybe Int
  , _optShowVersion :: Bool
  , _optShowHelp :: Bool }
  deriving (Show)

$(makeLenses ''Options)

defaultOptions :: Options
defaultOptions = Options
  { _optScale = 1
  , _optWidth = 500
  , _optHeight = 500
  , _optRotateX = 0.0
  , _optRotateY = 0.5
  , _optRotateZ = 0.0
  , _optOutputFormat = Just "stl-%03d.png"
  , _optOutputFolder = Just "output"
  , _optOutputFrames = Nothing
  , _optShowVersion = False
  , _optShowHelp = False }

(</) :: Read a => ASetter s t b (Maybe a) -> String -> Maybe String -> s -> t
f </ a = set f . readMaybe . fromMaybe a

(<|) :: Read b => ASetter s t a b -> b -> String -> s -> t
f <| a = set f . fromMaybe a . readMaybe

optionList :: [OptDescr (Options -> Options)]
optionList =
  [ Option "s" ["scale"]   (ReqArg scale "scale VALUE") "Scale solid to 1/s"
  , Option ""  ["rx"]      (ReqArg rx "rotate ANGLE") "Spin model on X coordinates each step by ANGLE"
  , Option ""  ["ry"]      (ReqArg ry "rotate ANGLE") "Spin model on Y coordinates each step by ANGLE"
  , Option ""  ["rz"]      (ReqArg rz "rotate ANGLE") "Spin model on Z coordinates each step by ANGLE"
  , Option "w" ["width"]   (ReqArg width "width VALUE") "Initial window width"
  , Option "h" ["height"]  (ReqArg height "height VALUE") "Initial window height"
  , Option "o" ["output"]  (OptArg output "output FILE") "Output to directory"
  , Option "f" ["format"]  (OptArg format "format STRING") "Format string for saving the output file"
  , Option "n" ["frames"]  (OptArg frames "frames COUNT") "Number of frames to output to file"
  , Option "h" ["help"]    (NoArg help) "Print this help message" 
  , Option "v" ["version"] (NoArg version) "Print version number" 
  ]
  where scale = optScale <| 1.0
        rx = optRotateX <| 0.0
        ry = optRotateY <| 0.5
        rz = optRotateZ <| 0.0
        width = optWidth <| 500
        height = optHeight <| 500
        format = optOutputFormat </ "stl-%03d.png"
        output = optOutputFolder </ "output"
        frames = optOutputFrames </ ""
        help = set optShowHelp True
        version = set optShowVersion True

arguments :: [String] -> IO (Options, [String])
arguments argv =
  case getOpt Permute optionList argv of
    (o, n, []) -> do
      let (config, files) = (foldl (flip id) defaultOptions o, n)
      if null files
        then do
          putStrLn "Missing input file"
          exitFailure
        else return (config, files)
    (_, _, errs) -> do
      putStrLn (concat errs ++ usageInfo header optionList)
      exitFailure
  where header = unlines ["Usage: stl-viewer [flags] FILE", "", "stl-viewer flags:"]
