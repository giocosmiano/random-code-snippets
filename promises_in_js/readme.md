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
