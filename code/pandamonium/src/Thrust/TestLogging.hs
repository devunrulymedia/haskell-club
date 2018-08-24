
import System.IO

main :: IO ()
main = do
  handle <- openFile "/tmp/outfile" WriteMode
  hPutStrLn handle "i am a banana"
  
