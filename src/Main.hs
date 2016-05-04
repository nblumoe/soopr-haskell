import System.Environment

main :: IO ()
main = getArgs >>= print . foo . head

foo s = "Fooo! " ++ s
