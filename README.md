# Pandoc Extensions for Devalot.com

The `devalot-pandoc` package can either be used as a Haskell library
or through the `devalot-pandoc` executable.  The `devalot-pandoc`
executable can act in a few different "modes", the primary one being a
Pandoc JSON filter.  See the output of `devalot-pandoc --help` for
more information.

I use this package to build [Devalot.com][], write [presentations][],
and author books.  The most recent of which is the forthcoming
[Effective Ruby][] book, published by Addison-Wesley.

## Extensions

### Inserting Code from External Files

Using [fenced code blocks][] you can insert code from an external file
into your markdown document.  This is done by using the `include` key
in the fenced code block attribute section:

    ~~~ {.ruby include="path/to/somefile.rb"}
    ~~~

Placing the above in your markdown file will instruct `devalot-pandoc`
to insert the contents of the `path/to/somefile.rb` file into the
document, replacing the existing fenced code block.  Furthermore, it's
possible to *narrow* what gets inserted using delimiters in the source
code.  For example, consider this version of `somefile.rb`:

~~~ {.ruby}
require('set')

class Foo
  # <<: cut
  def initialize
    @foo = Set.new
  end
  # :>>
end
~~~

Notice the commented delimiters `:>> cut` and `<<:`?  You can ask
`devalot-pandoc` to only insert code from `somefile.rb` which is
between those delimiters using the `token` attribute:

    ~~~ {.ruby include="path/to/somefile.rb" token="cut"}
    ~~~

### Executing Commands and Inserting Their Output

Using [fenced code blocks][] and the `exec` attribute you can execute
arbitrary commands and insert their output into the document,
replacing the original fenced code block.  The contents of the
existing fenced code block are used as the STDIN for the command:

    ~~~ {exec="date"}
    ~~~

    ~~~ {exec="sh -"}
    cd /some/path
    ls -l
    ~~~

### Experimental Features

There are other experimental features that you'll need to read the
source code to use:

  * A parser that reads configuration files that include a list of
    markdown file names.

  * The ability to read the configuration files mentioned above and
    stitch all of the listed markdown files into a single markdown
    file.  This is useful for combining the chapters of a book into a
    single file, for example.

  * Process source files and replace specially formatted comments with
    other text.  I use this to replace `# ...` with Unicode ellipses.

[fenced code blocks]: http://johnmacfarlane.net/pandoc/README.html#fenced-code-blocks
[devalot.com]: http://www.devalot.com/
[presentations]: https://github.com/devalot
[effective ruby]: http://www.effectiveruby.com/
