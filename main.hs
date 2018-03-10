import System.Environment 
import qualified Data.Text as T
main = do
	fileName <- getArgs 
	putStrLn $ head fileName
	text <- readFile $ head fileName
	parsedText <- T.words text
	putStrLn $ head fileName