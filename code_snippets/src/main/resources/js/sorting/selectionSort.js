/**
 * Haskell
 deleteFromOri :: Eq a => a -> [a] -> [a]
 deleteFromOri _ [] = []
 deleteFromOri x (y:ys)
 | x == y = ys
 | otherwise = y:deleteFromOri x ys

 selectionSort :: Ord a => [a] -> [a]
 selectionSort [] = []
 selectionSort xs = mini : selectionSort xs'
 where
 mini = minimum xs
 xs' = deleteFromOri mini xs
*/

const selectionSort = list => {
    if (! Array.isArray(list)) {
        throw Error("Error: Not an array");

    } else if (list.length === 0) {
        return list;
    }

    const deleteFromOri = (x, thisList) => {

        if (thisList.length === 0) {
            return thisList;
        }

        const y  = thisList[0]
            , ys = thisList.slice(1);

        if ( x === y ) {
            return ys;
        }

        return [ y, ...deleteFromOri( x, ys ) ];
    };

    const mini = Math.min(...list)
        , xs = deleteFromOri(mini, list)

    return [ mini, ...selectionSort( xs ) ] ;
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
    , results = selectionSort(unSortedArray)
    , endTime = new Date()
    , timeDiff = (endTime - startTime) / 1000
    , isArraySorted = isArrayEquals(sortedArray, results);

console.log(`unSortedArray:         ${unSortedArray}`);
console.log(`selectionSort Results: ${results}`);
console.log(`'selectionSort' with ${unSortedArray.length} elements --> isArraySorted: ${isArraySorted}, elapsedTime: ${timeDiff} secs. `);


