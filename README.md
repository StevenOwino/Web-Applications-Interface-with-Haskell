# Web-Applications-Interface-with-Haskell
WAI(Web Application Interface) 3.0 Rapid Prototyping and Fast production code 

The guide here works 100% on MacOs X 10.11.6 El Capital. If any trouble is encountered, learn what you dont know.
Setup your Haskell platform using "haskell_stack.txt". Kindly dont forget to install the "universe-base" package
Verify everything looks good: Run "stack ghci" on the command prompt, load the "Godel.hs" file in GHCi, and verify its source file enumeration in
*Main> map from [0..29] == take 30 ss

OUTPUT should be:
Configuring GHCi with the following packages: 
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /private/var/folders/xc/0zyxjyvn6fx4q17s5d8jrl_00000gn/T/haskell-stack-ghci/2a3bbd58/ghci-script
Prelude> :load Godel.hs
[1 of 1] Compiling Main             ( Godel.hs, interpreted )
Ok, modules loaded: Main.
*Main>  map from [0..29] == take 30 ss
True
*Main> 

We are now ready for the Yesod Web Framework, if your machine pulls through this test. Create a new folder and name it "Yesod", copy "bench.hs" to the folder and sudo chmod +x bench.hs
Now run "./bench.hs && ./bench --output bench.html" in the command prompt. Allow the download, installs GHCi version 8.0.1, and resolves to GHCi's snapshot lts7.14

In the command prompt, run  "stack install yesod", then 
Run "stack runghc helloworld.hs" 
On your web browser, enter http://localhost:3000 to get the web server running on port 3000. You should get the stdout "Hello World!"

REFERENCE: 
https://www.haskell.org/ghc/
https://docs.haskellstack.org/en/stable/README/#the-haskell-tool-stack
https://www.yesodweb.com/book/basics
