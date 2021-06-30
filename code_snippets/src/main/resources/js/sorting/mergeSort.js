/**
 * Haskell
 mergeSort :: Ord a => [a] -> [a]
 mergeSort [] = []
 mergeSort [x] = [x]
 mergeSort xs = merge firstHalf secondHalf
 where
 half = length xs `div` 2
 firstHalf  = mergeSort $ take half xs
 secondHalf = mergeSort $ drop half xs

 merge :: (Ord a) => [a] -> [a] -> [a]
 merge x [] = x
 merge [] y = y
 merge allX@(x:xs) allY@(y:ys)
 | x <= y    = x : merge xs allY
 | otherwise = y : merge allX ys
*/

const mergeSort = list => {
    if (! Array.isArray(list)) {
        throw Error("Error: Not an array");

    } else if (list.length <= 1) {
        return list;
    }

    const merge = tuple => {
        const xs = tuple.xs
            , ys = tuple.ys;

        if ( xs.length   > 0 && ys.length === 0) return xs;
        if ( xs.length === 0 && ys.length   > 0) return ys;

        if (xs[0] <= ys[0]) {
            return [ xs[0], ...merge( { xs: xs.slice(1), ys } ) ];
        }

        return [ ys[0], ...merge( { xs, ys: ys.slice(1) } ) ];
    };

    const half = Math.floor( list.length / 2 )
        , firstHalf  = mergeSort( list.slice(0, half) )
        , secondHalf = mergeSort( list.slice(half) );

    return merge( { xs : firstHalf, ys : secondHalf } );
};

// shallow array equality
const isArrayEquals = (a, b) =>
    Array.isArray(a) && Array.isArray(b) &&
    a.length === b.length &&
    a.every((val, index) => val === b[index]);

const generateArray = arrSize => Array.from({ length: arrSize }, (_, i) => i + 1);

const size = 15000 // maxing out to 15k as 20k will throw `RangeError: Maximum call stack size exceeded`
    , sortedArray = [...generateArray(size)]
    , unSortedArray = [...generateArray(size).sort((a, b) => b - a )]
    , startTime = new Date()
    , results = mergeSort(unSortedArray)
    , endTime = new Date()
    , timeDiff = (endTime - startTime) / 1000
    , isArraySorted = isArrayEquals(sortedArray, results);

console.log(`unSortedArray:     ${unSortedArray}`);
console.log(`mergeSort Results: ${results}`);
console.log(`'mergeSort' with ${unSortedArray.length} elements --> isArraySorted: ${isArraySorted}, elapsedTime: ${timeDiff} secs. `);


