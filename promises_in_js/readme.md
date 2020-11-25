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

Haskell λ > countWords "hello world, Hello There!!!"
[("hello",2),("there",1),("world",1)]
```

### Sample word count in Javascript using [RamdaJS](https://ramdajs.com/docs/#)
```javascript
import * as R from "ramda";

const wordCount = R.compose(R.map(w => w.length), R.groupBy(R.identity), R.sortBy(R.identity), R.map(R.replace(/\W/gi, "")), R.split(" "), R.toLower);

console.log(wordCount("hello world, Hello There!!!"));
// output ==> { hello : 2, there : 1, world : 1 }
```

### Sample word count in Java
```java
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public class WordCount {
    public static void main(String[] args) {

        String sample = "hello world, Hello There!!!";
        TreeMap<String, Integer> sortedMap =
                Arrays.stream(
                        Optional.ofNullable(sample)
                                .orElse("")
                                .toLowerCase()
                                .split(" "))
                        .map(e -> e.replaceAll("\\W", ""))
                        .collect(Collectors.groupingBy(Function.identity()))
                        .entrySet()
                        .stream()
                        .sorted(Map.Entry.comparingByKey())
                        .collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().size(),
                                (oldValue, newValue) -> oldValue, TreeMap::new))
                ;
        sortedMap.entrySet().forEach(System.out::println);
    }
}

// output ==> 
// hello=2
// there=1
// world=1
```

### Sample word count in [Scala](https://www.scala-lang.org/)
```scala worksheet

def wordCount(sentence: String): Unit = {
  Option(sentence)
    .map(e => e.toLowerCase)
    .map(e => e.split(" "))
    .toList
    .flatten
    .map(e => e.replaceAll("\\W", ""))
    .groupBy(identity)
    .map(e => e._1 -> e._2.size)
    .toSeq
    .sortBy(_._1)
    .foreach(e => println(e))
}

wordCount("hello world, Hello There!!!")
// output ==>
// (hello,2)
// (there,1)
// (world,1)
```
