/**
 * Haskell
 insert :: Ord a => a -> [a] -> [a]
 insert x [] = [x]
 insert x (y:ys)
 |  x < y = x:y:ys
 | otherwise = y : insert x ys

 insertionSort :: Ord a => [a] -> [a]
 insertionSort [] = []
 insertionSort (x:xs) = insert x (insertionSort xs)
*/

const insertionSort = list => {
    if (! Array.isArray(list)) {
        throw Error("Error: Not an array");

    } else if (list.length === 0) {
        return list;
    }

    const insert = (x, thisList) => {

        if (thisList.length === 0) {
            return [ x ];
        }

        const y  = thisList[0]
            , ys = thisList.slice(1);

        if ( x < y ) {
            return [ x, y, ...ys ];
        }

        return [ y, ...insert( x, ys ) ];
    };

    const x  = list[0]
        , xs = list.slice(1);

    return insert( x, insertionSort( xs ) );
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
    , results = insertionSort(unSortedArray)
    , endTime = new Date()
    , timeDiff = (endTime - startTime) / 1000
    , isArraySorted = isArrayEquals(sortedArray, results);

console.log(`unSortedArray:         ${unSortedArray}`);
console.log(`insertionSort Results: ${results}`);
console.log(`'insertionSort' with ${unSortedArray.length} elements --> isArraySorted: ${isArraySorted}, elapsedTime: ${timeDiff} secs. `);


