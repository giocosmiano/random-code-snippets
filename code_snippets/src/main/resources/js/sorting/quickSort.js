/**
 * Haskell
 quickSort :: Ord a => [a] -> [a]
 quickSort [] = []
 quickSort (x:xs) =
 let smallerOrEqual = filter (<= x) xs
 larger = filter (> x) xs
 in  quickSort smallerOrEqual ++ [x] ++ quickSort larger
*/

const quickSort = list => {
    if (! Array.isArray(list)) {
        throw Error("Error: Not an array");

    } else if (list.length === 0) {
        return list;
    }

    const x = list[0]
        , xs = list.slice(1)
        , smallerOrEqual = xs.filter(elem => elem <= x)
        , larger  = xs.filter(elem => elem > x);

    return [ ...quickSort(smallerOrEqual), x, ...quickSort(larger) ];
};

// shallow array equality
const isArrayEquals = (a, b) =>
    Array.isArray(a) && Array.isArray(b) &&
    a.length === b.length &&
    a.every((val, index) => val === b[index]);

const generateArray = arrSize => Array.from({ length: arrSize }, (_, i) => i + 1);

const size = 5000 // maxing out to 5k as 10k will throw `RangeError: Maximum call stack size exceeded`
    , sortedArray = [...generateArray(size)]
    , unSortedArray = [...generateArray(size).sort((a, b) => b - a )]
    , startTime = new Date()
    , results = quickSort(unSortedArray)
    , endTime = new Date()
    , timeDiff = (endTime - startTime) / 1000
    , isArraySorted = isArrayEquals(sortedArray, results);

console.log(`unSortedArray:     ${unSortedArray}`);
console.log(`quickSort Results: ${results}`);
console.log(`'quickSort' with ${unSortedArray.length} elements --> isArraySorted: ${isArraySorted}, elapsedTime: ${timeDiff} secs. `);


