# fword-haskell

Welcome! this is a brainf*** interpreter that I wrote in Haskell. 

### Usage

Make sure you have [haskell stack](https://docs.haskellstack.org/en/stable/README/) installed. After cloning, build the program with:

```stack build```

You can then run the program with:

```stack run -- [your filename here]```

For example, to run [the mandelbrot program I used to speed test](https://github.com/ErikDubbelboer/brainfuck-jit/blob/master/mandelbrot.bf), place the file in the source folder and type:

```stack run -- mandelbrot.bf```

### Info

This program has two interfaces. The first, located in `src/BF.hs`, stores the value of the cells in a custom zipper data structure inspired by [this article](http://learnyouahaskell.com/zippers). The second, located in `src/BF/Stateful.hs`, stores the value of the cells in a "real world" vector array and is much faster, although perhaps less "Haskell-y". By default, the program in `app/Main.hs` uses this second interface.

fword-haskell also contains a parser that I wrote using [Parsec](https://hackage.haskell.org/package/parsec), located in `src/BF/Parsing.hs`. Originally the program processed instructions by moving around a string of brainf*** characters. I saw a massive speed improvement when I switched to my parser and the "Instruction" data type. Parsec also includes pretty error messages that make this program feel like a real interpreter :)
