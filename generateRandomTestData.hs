
import Test.QuickCheck
import System.Random
import Control.Monad (replicateM_)


loglevel :: Gen String
loglevel = elements ["WARN", "FATAL", "DEBUG", "INFO"]

timestamp :: Gen Integer
timestamp = choose (14143,54143)

message :: Gen String
message = oneof [loggedout, loggedin, requested]

loggedout :: Gen String
loggedout = do
    u <- user
    return (u ++ " logged out")

loggedin :: Gen String
loggedin = do
    u <- user
    return (u ++ " logged in")

requested :: Gen String
requested = do
    u <- user
    p <- page
    return (u ++ " requested " ++ p)

user :: Gen String
user = elements [
    "feugiat.nec.diam@Praesentinterdum.org",
    "montes.nascetur.ridiculus@nequeSedeget.net",
    "Cras.convallis@nuncrisus.co.uk",
    "vulputate@anteipsumprimis.ca",
    "sodales.purus.in@nequesedsem.edu",
    "porttitor@Duisat.org",
    "taciti.sociosqu@sit.com",
    "interdum@lectus.edu",
    "libero.Integer.in@Proinnon.co.uk",
    "et@Suspendissecommodo.net",
    "mauris.rhoncus@lorem.org",
    "accumsan.laoreet@turpisnonenim.net",
    "nascetur.ridiculus.mus@sollicitudin.ca",
    "neque.venenatis.lacus@amet.co.uk",
    "augue@tristiquepellentesque.org",
    "iaculis.enim.sit@tellusjusto.co.uk",
    "fringilla.euismod.enim@Praesenteu.ca",
    "semper.erat.in@eleifendnondapibus.co.uk",
    "at@fermentumfermentumarcu.org",
    "velit.Quisque@luctusut.edu",
    "ullamcorper.velit@convallis.ca",
    "non.ante@interdumlibero.co.uk",
    "Nullam.vitae@et.org",
    "egestas@Vivamus.com",
    "et.ipsum.cursus@infaucibusorci.ca",
    "Ut.sagittis@noncursusnon.ca",
    "In.condimentum.Donec@diam.ca",
    "Cras.dolor@accumsanlaoreetipsum.ca",
    "eleifend.vitae@leoVivamus.com",
    "justo@Sed.org",
    "Nam.ac.nulla@consequat.net",
    "magna@Nullaeuneque.org",
    "nunc.ac@arcu.org",
    "sem@Maurismolestie.com",
    "orci.consectetuer.euismod@nuncsedpede.ca",
    "nisi.Cum.sociis@venenatisa.co.uk",
    "Donec@Nunclaoreetlectus.co.uk",
    "Integer.sem@necorci.edu",
    "consectetuer.cursus.et@aliquetsem.edu",
    "facilisis@sagittisaugue.ca",
    "Quisque.varius@atpretium.com",
    "nunc@Integerid.ca",
    "in.aliquet@Sedpharetra.org",
    "ultricies.adipiscing@metusurnaconvallis.net"
    ]

page :: Gen String
page = elements [
    "index.html",
    "public.html",
    "secret.html",
    "api.json",
    "../../../../all_passwords.txt"
    ]

garbage :: IO String
garbage = generate $ vectorOf 15 $ choose ('!', 'z')

logline :: IO String
logline = do
    t <- generate timestamp
    l <- generate loglevel
    m <- generate message
    return $ (show t) ++ " " ++ l ++ " " ++ m

line :: IO String
line = do
    randomNumber <- randomRIO (0,100) :: IO Integer
    if randomNumber > 90 then garbage else logline

main :: IO ()
main = replicateM_ 500 (line >>= putStrLn)