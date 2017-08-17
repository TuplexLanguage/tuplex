<a href="{{ site.baseurl }}/essential_features.html">Essential Features</a>

### Guides

* <a href="{{ site.baseurl }}/build_howto.html">Building the Compiler</a>
* <a href="{{ site.baseurl }}/syntax_intro.html">Syntax Introduction</a>
* <a href="{{ site.baseurl }}/formatting_strings.html">Formatting Strings</a>
* <a href="{{ site.baseurl }}/ranges.html">Ranges</a>
* <a href="{{ site.baseurl }}/advanced_types.html">Advanced Types</a>

### Blog Entries

<ul>
  {% for post in site.posts %}
    <li>
      <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a> <!--{{ post.date | date: "%y-%m-%d" }}-->
    </li>
  {% endfor %}
</ul>