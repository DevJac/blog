----
title: Functional Reactive Programming with `reactive-banana` and `vty`
----

I've recently been experimenting with Haskell's [`reactive-banana`][1], a functional reactive programming (FRP) library. I couldn't get the [`reactive-banana` examples][2] to work because of issues with `wxWidgets`, so I used [`vty`][3] instead. `vty` has fewer dependencies, and will provide a good demonstration of how to connect `reactive-banana` to a GUI library yourself.

## A Quick Introduction to `vty`

[`vty`][3] is a terminal GUI library similar to `ncurses`. By using a simple terminal GUI we will be able to spend more time focused on `reactive-banana` itself, but first we need to learn a little about `vty`.

The following code is a complete `vty` program which will print `Event`s as keys are pressed. The program ends after 10 seconds, and **will not respond to Ctrl-C or other signals**, so just wait it out. While the program runs, experiment by pressing a variety of keys.

```haskell
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified Graphics.Vty as V

main :: IO ()
main = do
    vty <- V.mkVty V.defaultConfig
    _ <- forkIO $ forever $ loop vty
    threadDelay $ 10 * 1000000
    V.shutdown vty

loop :: V.Vty -> IO ()
loop vty = do
    e <- V.nextEvent vty
    showEvent e
  where
    showEvent :: V.Event -> IO ()
    showEvent = V.update vty . V.picForImage . V.string V.defAttr . show
```

A few notes about the code:

- The `main` function initializes `vty` and starts the `loop` function in a separate thread, and then ends the entire program after 10 seconds. In Haskell all threads end when the main thread ends.

- The `loop` function receives an `V.Event` from `vty` and then prints it over and over in a loop. The `V.nextEvent` function blocks the thread until the next `V.Event` fires. In this case, all `V.Event`s are caused by keypresses.

- The `showEvent` function is a bit complicated due to `vty`'s API. Basically, `V.update` renderes "images" to the screen, and images are made up of "pictures", and `V.String V.defAttr` is a partially applied function that makes pictures. Note that `V.defAttr` is short for "*default* attr" (rather than "*define* attr") and causes `V.string` to use the default terminal colors.

- It's not necissary to use multiple threads with `vty`, and it's probably best to use a single thread in most cases. In this case though, that second thread will end up being useful when we integrate with `reactive-banana`.

For the specifics check out the [`vty` API documentation on Hackage][3].

With just a few lines of code we have a real-time interactive program. Now we can experiment with functional reactive programming.

## Connecting `vty` to `reactive-banana`

Before we connect `reactive-banana` to `vty` we need a high-level understanding of how `reactive-banana` works. What exactly does `reactive-banana` do? This might sound a little underwhelming, but bear with me; `reactive-banana` allows you to express IO actions in terms of input events. It's simply an alternative way of implementing an `Events -> IO ()` function! Except, `Events -> IO ()` isn't always simple; who know's what state is hiding in there or how that state is structured? It can be anything. Functional reactive programming gives us a standard way to express the logic and manage the state inside a conceptual `Events -> IO ()` function.

In `reactive-banana` we will built a "network" out of "`Events`" and "`Behaviors`" and about ten primative combinators. The conceptual network formed by these `Events`, `Behaviors`, and combinators is called an "event graph", although there is no type by this name. An "event graph" can be connected to the real world in a variety of ways, but the implementation will stay the same, this allows us to build abstract components in `reactive-banana` without worrying about how they will be connected to the real world. When the "event graph" is ultimately connected to real world inputs and outputs it becomes an "`EventNetwork`". Below we will build an extremely simple `EventNetwork` and use it in our program. There is no benefit to such a simple `EventNetwork`, but it will allow us to see how an `EventNetwork` is connected to the rest of the program. We will later build more sophisticated behaviors inside the `EventNetwork` without needing to alter the surrounding code much.

```haskell
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified Graphics.Vty as V
import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
    vty <- V.mkVty V.defaultConfig
    reactiveBananaNetwork <- network vty
    actuate reactiveBananaNetwork
    threadDelay $ 10 * 1000000
    V.shutdown vty

network :: V.Vty -> IO EventNetwork
network vty = compile $ do
    (inputE, fireInputE) <- newEvent
    _ <- liftIO $ forkIO $ forever $ fireInputE =<< V.nextEvent vty
    reactimate $ fmap showEvent inputE
  where
    showEvent :: V.Event -> IO ()
    showEvent = V.update vty . V.picForImage . V.string V.defAttr . show
```

About the code:

- `reactive-banana` and `vty` both have a type called `Event`. Do not get them mixed up. A `vty` `Event` is always something like "a key was pressed", while a `reactive-banana` `Event` is more general in meaning like "something happened" where you're able to choose what that "something" is. The type of `inputE` is `Event V.Event`, which means it's a `reactive-banana` `Event` containing a `vty` `Event`; we might think of it as "something happened, and that something was a key being pressed". Remember that "Event" is an overloaded term in these examples.

- In the `main` function we create a `reactiveBananaNetwork` which has the type `EventNetwork`. In `reactive-banana` there are conceptual "event graphs" which contain logic and state and are isolated from the real world; when these "event graphs" are combined with the real world by connecting inputs and outputs they become an `EventNetwork`.

- In the main function `actuate` runs the `EventNetwork`, with all its inputs and outputs, in a separate thread.

- In the network function `newEvent` creates a `reactive-banana` `Event` named `inputE`. `Events` and `Behaviors` are the two basic types that make up an FRP network. We will talk more about them later. The `fireInputE` should be called from outside the FRP network, and will cause the `inputE` inside the network to fire.

- `fireInputE =<< V.nextEvent vty` runs in an infinite loop. It repeatedly calls `V.nextEvent` which blocks until a `vty` `Event` occurs, and when an `Event` occurs it fires that `Event` into the FRP network. This is necessary because `reactive-banana` doesn't have a way to poll a blocking function like `V.nextEvent`, but we can make our own adapter with this one line. 

- `reactimate` is how the FRP network runs output actions.

## Building Logic with `reactive-banana`

Finally, I'll end with a more complex example. This program allows you to type in your terminal, and pressing escape will toggle capitalization of the letters.

```haskell
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Data.Char (toUpper)
import qualified Graphics.Vty as V
import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
    vty <- V.mkVty V.defaultConfig
    actuate =<< network vty
    threadDelay $ 20 * 1000000
    V.shutdown vty

update :: V.Vty -> [String] -> IO ()
update vty = V.update vty . V.picForImage . mconcat . fmap (V.string V.defAttr)

network :: V.Vty -> IO EventNetwork
network vty = compile $ do
    (inputEvents, fireInputEvent) <- newEvent
    _ <- liftIO $ forkIO $ forever $ fireInputEvent =<< V.nextEvent vty
    outputEvents <- liftMoment $ pureNetwork inputEvents
    reactimate' =<< changes (fmap (update vty) outputEvents)

isChar :: V.Event -> Bool
isChar (V.EvKey (V.KChar _) _) = True
isChar _                       = False

isEsc :: V.Event -> Bool
isEsc (V.EvKey (V.KEsc) _) = True
isEsc _                    = False

mightCapitalize :: Bool -> String -> String
mightCapitalize True  = fmap toUpper
mightCapitalize False = id

pureNetwork :: Event V.Event -> Moment (Behavior [String])
pureNetwork inputEvents = do
    let inputChar = (\(V.EvKey (V.KChar c) _) -> pure c) <$>
                    filterE isChar inputEvents
    let inputEsc = filterE isEsc inputEvents
    accumedString <- accumB "" ((\new accumed -> accumed ++ new) <$> inputChar)
    capitalizeSwitch <- accumE False (not <$ inputEsc)
    maybeCapitalize <- switchB (pure $ mightCapitalize False)
                               (pure . mightCapitalize <$> capitalizeSwitch)
    pure $ fmap pure $ maybeCapitalize <*> accumedString
```

The core logic and state is managed in `pureNetwork`.

You can't tell by reading, but it's been over 2 years since I started writing this post. I lost momentum, and then lost interest in publishing a blog until recently. I've lost most of the insights I had to offer, so I will instead refer you to the [`reactive-banana` documentation][4]. `reactive-banana` is relatively small compared to other FRP frameworks, and has good documentation.

Hopefully this small self-contained example will give you a starting place for your own experiments. As an exercise, modify the program so that toggling capitalization is throttled to once per second, but keypresses should remain unthrottled. As a hint, see [`fromPoll`][5].

[1]: https://hackage.haskell.org/package/reactive-banana
[2]: https://wiki.haskell.org/Reactive-banana/Examples
[3]: https://hackage.haskell.org/package/vty
[4]: https://hackage.haskell.org/package/reactive-banana/docs/Reactive-Banana-Combinators.html
[5]: https://hackage.haskell.org/package/reactive-banana/docs/Reactive-Banana-Frameworks.html#v:fromPoll
