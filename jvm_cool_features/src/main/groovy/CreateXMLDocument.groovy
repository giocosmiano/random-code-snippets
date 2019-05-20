def langs = ['C++' : 'Stroustrup', 'Java' : 'Gosling', 'Lisp' : 'McCarthy', 'Ruby' : 'Matz']

def bldr = new groovy.xml.MarkupBuilder()

bldr.languages {
  langs.each {key, value ->
    language(name: key) { author(value) }
  }
}

/*
<languages>
  <language name='C++'>
    <author>Stroustrup</author>
  </language>
  <language name='Java'>
    <author>Gosling</author>
  </language>
  <language name='Lisp'>
    <author>McCarthy</author>
  </language>
  <language name='Ruby'>
    <author>Matz</author>
  </language>
</languages>
*/
