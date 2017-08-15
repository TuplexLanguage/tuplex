<a href="{{ site.baseurl }}/essential_features.html">Essential Features</a>

### Guides

* <a href="{{ site.baseurl }}/build_howto.html">Building the Compiler</a>
* <a href="{{ site.baseurl }}/syntax_intro.html">Syntax Introduction</a>

### Blog Entries

<ul>
  {% for post in site.posts %}
    <li>
      <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a> <!--{{ post.date | date: "%y-%m-%d" }}-->
    </li>
  {% endfor %}
</ul>