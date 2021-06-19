// https://ramdajs.com/docs/#
// https://www.npmjs.com/package/ramda

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

const text = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.";

console.log(wordCount(text));
