- [ReactiveX - Async Programming with Observable Streams](http://reactivex.io/)
  - [rxjs](https://github.com/ReactiveX/rxjs)
  - [RxJS API](https://rxjs-dev.firebaseapp.com/api)
  - [RxJS Overview](https://rxjs-dev.firebaseapp.com/guide/overview)
  - [Learn RxJS](https://www.learnrxjs.io/)
  - [Angular RxJS](https://angular.io/guide/rx-library)

- Use [nvm](https://github.com/nvm-sh/nvm) to switch between node/npm versions
  - [How to install Node.js on Ubuntu 16.04/18.04 using NVM](https://hackernoon.com/how-to-install-node-js-on-ubuntu-16-04-18-04-using-nvm-node-version-manager-668a7166b854)
  - [Installing Node.js with nvm to Linux & macOS & WSL](https://gist.github.com/d2s/372b5943bce17b964a79)
```
nvm ls
->      v6.17.0
        v8.16.0
       v10.15.0
```

```
nvm use v8.16.0
```

- To run the samples in the directories using `rxjs v6.5.2`

```
node ./sample.js
```

### Sample word count in [Haskell](https://www.haskell.org/)
```haskell
import Data.Char
import Data.List

--
-- countWords :: String -> [(String,Int)]
-- countWords = map (\w -> (head w, length w)) . group . sort . words . filter (\x -> isAlphaNum x || isSpace x) . map toLower
--

Haskell λ > countWords "hello world,\n\t Hello There!!!"
[("hello",2),("there",1),("world",1)]
```

### Sample word count in Javascript using [RamdaJS](https://ramdajs.com/docs/#)
```javascript
import * as R from "ramda";

const wordCount =
    R.compose(
        R.map(w => w.length)
        , R.groupBy(R.identity)
        , R.sortBy(R.identity)
        , R.map(R.replace(/\W/gi, ""))
        , R.filter(e => ! R.isEmpty(e))
        , R.split(/\s/)
        , R.toLower
        , R.defaultTo("") // set to empty string if null/undefined
    );

const useDefault = Math.random() > 0.50
, message = useDefault ? "hello world,\n\t Hello There!!!" : null;

wordCount(message);

// output ==> { "hello" : 2, "there" : 1, "world" : 1 }
// output ==> { }  // if message is null
```

### Sample word count in Java
```java
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public class WordCount {
    public static void main(String[] args) {

        boolean useDefault = Math.random() > 0.50;
        String message = useDefault ? "hello world,\n\t Hello There!!!" : null;
        TreeMap<String, Integer> sortedMap =
                Arrays.stream(
                        Optional.ofNullable(message)
                                .orElse("")
                                .toLowerCase()
                                .split("\\s"))
                        .filter(e -> Objects.nonNull(e) && e.length() > 0 && ! e.chars().allMatch(Character::isWhitespace))
                        .map(e -> e.replaceAll("\\W", ""))
                        .collect(Collectors.groupingBy(Function.identity()))
                        .entrySet()
                        .stream()
//                        .sorted(Map.Entry.comparingByKey())  // no need as it returns a TreeMap sorted by keys
                        .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().size(),
                                (oldValue, newValue) -> oldValue, TreeMap::new))
                ;
        System.out.println(sortedMap);
    }
}

// output ==> { hello=2, there=1, world=1 }
// output ==> { }  // if message is null
```

### Sample word count in [Scala](https://www.scala-lang.org/)
```scala
import scala.collection.breakOut
import scala.collection.immutable.TreeMap

// using `breakout` to transform `map` to `treeMap`
// https://docs.scala-lang.org/tutorials/FAQ/breakout.html
def wordCount(sentence: String): TreeMap[String, Int] = {
  Option(sentence)
    .orElse(Some(""))
    .map(e => e.toLowerCase)
    .map(e => e.split("\\s"))
    .toList
    .flatten
    .filter(_.trim.nonEmpty)
    .map(e => e.replaceAll("\\W", ""))
    .groupBy(identity)
    .map(e => e._1 -> e._2.size)(breakOut)
//    .toSeq  // no need as it returns a TreeMap sorted by keys
//    .sortBy(_._1) 
//    .toMap
}

val useDefault = Math.random() > 0.50
val message = if (useDefault) "hello world,\n\t Hello There!!!" else null
println(wordCount(message))

// output ==> Map( hello -> 2, there -> 1, world -> 1 )
// output ==> Map()  // if message is null
```

### [Combinator Birds](https://www.angelfire.com/tx4/cus/combinator/birds.html) in [Haskell](https://hackage.haskell.org/package/data-aviary-0.4.0/docs/Data-Aviary-Birds.html)
  - [Haskell Data.Aviary.Birds Docs](https://hackage.haskell.org/package/data-aviary-0.4.0/docs/src/Data-Aviary-BirdsInter.html)  
  - [Haskell Data.Aviary.Birds Src](https://hackage.haskell.org/package/data-aviary-0.4.0/src/src/Data/Aviary/Birds.hs)
```javascript
// Identity Bird (aka Idiot)
// Haskell λ > :t id
I = a => a

// Kestrel (Corresponds to the encoding of true in the lambda calculus)
// Haskell λ > :t const
// e.g. K(5)(3) === 5
K = a => b => a

// Bluebird
// Haskell λ > :t (.)
// (.) :: (b -> c) -> (a -> b) -> a -> c
// e.g. B(a => a % 2 === 0)(a => a * 3)(2) === true
// e.g. B(a => a % 2 === 0)(a => a * 3)(3) === false
B = f => g => x => f (g (x))

// Cardinal
// Haskell λ > :t flip
// e.g. C(a => b => a ** b)(5)(3) === 243
// e.g. C(a => b => a ** b)(3)(5) === 125
C = f => a => b => f (b) (a)

// I* -> Apply/Application
// Haskell λ > :t ($)
// e.g ISTAR(a => a * 3)(7) === 21
ISTAR = f => a => f (a)

// Kite (Corresponds to the encoding of false in the lambda calculus)
// Haskell λ > :t flip const
// e.g. KI(5)(3) === 3
KI = K(I)

// Starling -> Haskell Applicative's (<*>) on functions
// Haskell λ > :t (<*>)
// (<*>) :: Applicative f => f (a -> b) -> f a -> f b
// OR
// Haskell λ > s :: (a -> b -> c) -> (a -> b) -> a -> c
// Haskell λ > s f g x = (f x) (g x)
S = f => g => x => (f (x)) (g (x))

// Thrush -> Reverse application
// Haskell λ > :t flip id
// e.g. T(5)(x => x ** 3) === 125
// e.g. T(5)(x => x % 2 === 0) === false
T = a => f => f (a)

// Vireo -> Pair of arguments
// Haskell λ > :t flip . flip id
// e.g. V(3)(5)(a => b => a ** b) === 243
// e.g. V(5)(3)(a => b => a ** b) === 125 
V = a => b => f => f (a) (b)

// Mockingbird -> Self application 
M = f => f (f)
```

- Additional References
  - [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
  - [Combinatory Logic](https://en.wikipedia.org/wiki/Combinatory_logic)
  - [SKI Combinator Calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
  - [To Dissect a Mockingbird](https://dkeenan.com/Lambda/index.htm)
  - [Lambda Calculus and Combinatory Logic](https://softoption.us/node/654)  
  - [Combinatory Logic I](https://softoption.us/node/45)
  - [SKI Combinator Calculus](https://people.cs.uchicago.edu/~odonnell/Teacher/Lectures/Formal_Organization_of_Knowledge/Examples/combinator_calculus/)  
  - [Reader monad and SKI combinators](https://kseo.github.io/posts/2016-12-24-reader-monad-and-ski-combinators.html)
