---
---
<a href="{{ site.baseurl }}/index.html">Tuplex Home</a>

### Guides

<a href="{{ site.baseurl }}/build_howto.html">Building the Compiler</a>

<a href="{{ site.baseurl }}/syntax_intro.html">Syntax Introduction</a>

### Blog Entries

{% for post in site.posts %}
<p><a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a> <!--{{ post.date | date: "%y-%m-%d" }}--></p>
{% endfor %}
