----
title: Functional Reactive Programming with `reactive-banana` and `vty`
----

I recently built some small but interesting demos using `reactive-banana` and `vty`. Although [reactive-banana has its own examples][1], I haven't been able to get them to work because of issues with `wxWidgets`. Even thought these few examples wont cover the same amount of material as the [reactive-banana examples][1] they will be enough to get you started with `reactive-banana` and functional reactive programming (FRP). I hope to teach you enough that you'll feel comfortable using the [`reactive-banana` documentation][2] to learn the rest of the library.

### A Quick Introduction to `vty`

[`vty` describes itself][3] as a "terminal GUI library in the niche of ncurses. It is intended to be easy to use, have no confusing corner cases, and good support for common terminal types." By using a simple terminal GUI we will be able to spend more time focused on `reactive-banana` itself, but first we need to learn a little about `vty`.

The following code is a complete `vty` program which will print the corresponding `Event` as keys are pressed. The program ends after 10 seconds, and will not respond to Ctrl-C or other signals.

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

- The `main` function starts a initializes `vty` and starts the `loop` function, and then ends the entire program after 10 seconds. In Haskell all threads end when the main thread ends.

- The `loop` function receives an `V.Event` from `vty` and then prints it over and over in a loop. The `V.nextEvent` function blocks the thread until the next `V.Event` fires because the user pressed a key.

- The `showEvent` function is a bit complicated due to `vty`'s API. Basically, `V.update` renderes "images" to the screen, and images are made up of "pictures", and `V.String V.defAttr` is a partially applied function that makes pictures. Note that `V.defAttr` is short for "default attr" and causes `V.string` to use the default terminal colors.

- It's not necissary to use multiple threads with `vty`, and it's probably best to use a single thread in most cases. In this case though, that second thread will end up being useful when we integrate with `reactive-banana`.

For the specifics check out the [`vty` API documentation on Hackage][3].

With just a few lines of code we have a real-time interactive program. Now we're ready for some functional reactive programming. Let's see what it can do for us.

### Connecting `vty` to `reactive-banana`

Before we connect `reactive-banana` to `vty` we need a high-level understanding of how `reactive-banana` works. What exactly does `reactive-banana` do? This might sound a little underwhelming, but bear with me; `reactive-banana` allows you to express IO actions in terms of input events. It's simply an alternative way of implementing a `Events -> IO ()` function! Except, `Events -> IO ()` isn't always simple; who know's what state is hiding in there or how that state is structured? It can be anything. Functional reactive programming gives us a standard way to express the logic and manage the state inside a conceptual `Events -> IO ()` function.

In `reactive-banana` we will built a "network" out of "`Events`" and "`Behaviors`" and about ten primative combinators. The conceptual network formed by these `Events`, `Behaviors`, and combinators is called an "event graph", although there is no type by this name. A "event graph" can be connected to the real world in a variety of ways, but the internal logic, code, and state management would stay the same, this allows us to build abstract components in `reactive-banana` without worrying about how they will be connected to the real world. When the "event graph" is ultimately connected to real world inputs and outputs it becomes an "`EventNetwork`". Below we will build an extremely simple `EventNetwork` and use it in our program. There is no benefit to such a simple `EventNetwork`, but it will allow us to see how an `EventNetwork` is connected to the rest of the program. We will later build more sophisticated behaviors inside the `EventNetwork` without needing to alter the surrounding code much.

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

- In the `main` function we create an `reactiveBananaNetwork` which has the type `EventNetwork`. In `reactive-banan` there are conceptual "event graphs" which contain logic and state and are isolated from the real world; when these "event graphs" are combined with real world input events and output actions they become an `EventNetwork`.

- In the main function `actuate` runs the `EventNetwork`, with all it's inputs and outputs, in a seperate thread.

- In the network function `newEvent` creates a `reactive-banana` `Event` named `inputE`. `Events` and `Behaviors` are the two basic types that make up an FRP network. We will talk more about them later. The `fireInputE` should be called from outside the FRP network, and will cause the `inputE` inside the network to fire.

- `fireInputE =<< V.nextEvent vty` runs in an infinite loop. It repeatedly calls `V.nextEvent` which blocks until a `vty` `Event` occurs (see the following bullet point), and when an `Event` occurs it fires that `Event` into the FRP network. This is necissary because `reactive-banana` doesn't have any way of directly polling blocking functions like this, but we can make our own adapter with this one line. 

- `reactive-banana` and `vty` both have a type called `Event`. Do not get them mixed up. A `vty` `Event` is always something like "a key was pressed", while a `reactive-banana` `Event` is more general in meaning like "something happened" where your able to choose what that "something" is. The type of `inputE` is `Event V.Event`, which means it's a `reactive-banana` `Event` containing a `vty` `Event`; we might think of it as "something happened, and that something was a key being pressed". Remember that "Event" is an overloaded term in these examples.

- `reactimate` is how the FRP network runs output actions. We will talk about the reason we're using `fmap` in that same line in the next section.

### Building Logic with `reactive-banana`

!!!*TODO*!!! Teach Events, Behaviors, and the 10 primative combinators.

### Additional Notes and Thanks

!!!*TODO*!!! Thank /r/haskellquestions user who helped me.
!!!*TODO*!!! Mention flapjax documentation.
!!!*TODO*!!! Mention the frp-zoo.

[1]: https://wiki.haskell.org/Reactive-banana/Examples
[2]: http://hackage.haskell.org/package/reactive-banana-1.1.0.1/docs/Reactive-Banana.html
[3]: https://hackage.haskell.org/package/vty
