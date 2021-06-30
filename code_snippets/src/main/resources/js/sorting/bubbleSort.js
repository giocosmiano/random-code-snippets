/**
 * Haskell
 swaps :: Ord a => [a] -> [a]
 swaps [] = []
 swaps [x] = [x]
 swaps (x1:x2:xs)
 | x1 > x2 = x2 : swaps(x1:xs)
 | otherwise = x1 : swaps(x2:xs)

 bubbleSort :: Ord a => [a] -> [a]
 bubbleSort [] = []
 bubbleSort xs
 | swaps xs == xs = xs -- did not change, stop
 | otherwise = bubbleSort $ swaps xs
*/

const bubbleSort = list => {
    if (! Array.isArray(list)) {
        throw Error("Error: Not an array");

    } else if (list.length === 0) {
        return list;
    }

    const swaps = thisList => {

        if (thisList.length <= 1) {
            return thisList;
        }

        const x1 = thisList[0]
            , x2 = thisList[1]
            , xs = thisList.slice(2);

        if ( x1 > x2 ) {
            return [ x2, ...swaps( [x1, ...xs] ) ];
        }

        return [ x1, ...swaps( [x2, ...xs] ) ];
    };

    if (isArrayEquals(swaps(list), list)) {
        return list;
    }

    return bubbleSort( swaps(list) );
};

// shallow array equality
const isArrayEquals = (a, b) =>
    Array.isArray(a) && Array.isArray(b) &&
    a.length === b.length &&
    a.every((val, index) => val === b[index]);

const generateArray = arrSize => Array.from({ length: arrSize }, (_, i) => i + 1);

const size = 2000 // maxing out to 2k as 5k will throw `RangeError: Maximum call stack size exceeded`
    , sortedArray = [...generateArray(size)]
    , unSortedArray = [...generateArray(size).sort((a, b) => b - a )]
    , startTime = new Date()
    , results = bubbleSort(unSortedArray)
    , endTime = new Date()
    , timeDiff = (endTime - startTime) / 1000
    , isArraySorted = isArrayEquals(sortedArray, results);

console.log(`unSortedArray:      ${unSortedArray}`);
console.log(`bubbleSort Results: ${results}`);
console.log(`'bubbleSort' with ${unSortedArray.length} elements --> isArraySorted: ${isArraySorted}, elapsedTime: ${timeDiff} secs. `);


