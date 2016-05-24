# xpathdsv


Extract DSV text from HTML and XML using XPATH expressions.

Example:

    xpathdsv $ xpathdsv  '//a'  '/a/text()' '/a/@href/text()' < ~/test.html


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
