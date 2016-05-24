# xpathdsv


Extract DSV text from HTML and XML using XPATH expressions.

Available on Hackage.

## Example

If you have an HTML file like this:

sample.html

```html
<html>
  <head><title>Test</title></head>
  <body>
    <h1>Some links</h1>
    <ul>
      <li><a href="http://news.ycombinator.com">Hacker News</a></li>
      <li><a href="http://yahoo.com">Yahoo</a>
      <li><a href="http://duckduckgo.com">Duck Duck Go</a>
      <li><a href="http://github.com">GitHub</a>
    </ul>
  </body>
</html>
```

You can extract a list of tab-separated values like this:

    xpathdsv  '//a'  '/a/text()' '/a/@href/text()' < sample.html

Output:

    Hacker News	http://news.ycombinator.com
    Yahoo	http://yahoo.com
    Duck Duck Go	http://duckduckgo.com
    GitHub	http://github.com


The first XPATH expression in the command sets the base node on which all the
following XPATH expressions are applied. Each of the following XPATH expressions
then generate a column of the row of data.

If you don't specify a `text()` node at the end of an XPATH expression, you'll get 
a string representation of a node, which may be useful for debugging:

     xpathdsv '//a' '/a' < sample.html

Output:

    <a href="http://news.ycombinator.com">Hacker News</a>
    <a href="http://yahoo.com">Yahoo</a>
    <a href="http://duckduckgo.com">Duck Duck Go</a>
    <a href="http://github.com">GitHub</a>

## Usage


    xpathdsv
    
    Usage: xpathdsv [--xml] [-F OUTPUT-DELIM] [-n NULL-OUTPUT] BASE-XPATH
                    [CHILD-XPATH]
      Extract DSV data from HTML or XML with XPath
    
    Available options:
      -h,--help                Show this help text
      --xml                    Parse as XML, rather than HTML.
      -F OUTPUT-DELIM          Default \t
      -n NULL-OUTPUT           Null value output string. Default ""
    
    See https://github.com/danchoi/xpathdsv for more information.


## Author

Daniel Choi <https://github.com/danchoi>


## References

* <https://en.wikipedia.org/wiki/XPath>
