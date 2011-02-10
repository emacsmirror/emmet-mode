# About Zencoding

[Description
here](http://www.456bereastreet.com/archive/200909/write_html_and_css_quicker_with_with_zen_coding/)
and [here](http://mondaybynoon.com/2009/08/17/the-art-of-zen-coding-bringing-snippets-to-a-new-level/).

I’ll quote the blog:

> zen-coding includes an entirely new angle to writing markup, and it
> facilitates the feature by letting you write HTML based on CSS
> selectors. It’s so simple it’s confusing at first. I think it’s best
> explained by doing a quick before and after. If you were to type:

    div#name.one.two

> and follow that with the zen-coding plugin keystroke (CMD+E in
  TextMate), the plugin will reformat the line as:

    <div id="name" class="one two"></div>

See the [EmacsWiki for more background on this mode.](http://www.emacswiki.org/emacs/ZenCoding)

# Screenshots and videos

* ![Picture of zencoding in action](http://img703.imageshack.us/img703/4474/zencodingmode.png)

* [YouTube video](http://www.youtube.com/watch?v=u2r8JfJJgy8)

# Installation

Just make sure zencoding-mode.el is in your `load-path`, if you
extracted zencoding-mode to a directory:

    (add-to-list "~/emacs.d/zencoding-mode")

And then just require as normal:

    (require 'zencoding-mode)

# Usage

Enable it by running `M-x zencoding-mode`. You probably want to add it
to auto-load on your sgml modes:

    (add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

Good to go.

# Examples

## Basic tags

    a                      <a></a>
    a.x                    <a class="x"></a>
    a#q.x                  <a id="q" class="x"></a>
    a#q.x.y.z              <a id="q" class="x y z"></a>
    #q                     \n<div id="q">\n</div>
    .x                     \n<div class="x">\n</div>
    #q.x                   \n<div id="q" class="x">\n</div>
    #q.x.y.z               \n<div id="q" class="x y z">\n</div>

## Empty tags

    a/                     <a/>
    a/.x                   <a class="x"/>
    a/#q.x                 <a id="q" class="x"/>
    a/#q.x.y.z             <a id="q" class="x y z"/>

## Self-closing tags

    input type=text        \n<input type="text"/>
    img                    \n<img/>
    img>metadata/*2        \n<img>\n<metadata/>\n<metadata/>\n</img>

## Siblings

    a+b                    <a></a><b></b>
    a+b+c                  <a></a><b></b><c></c>
    a.x+b                  <a class="x"></a><b></b>
    a#q.x+b                <a id="q" class="x"></a><b></b>
    a#q.x.y.z+b            <a id="q" class="x y z"></a><b></b>
    a#q.x.y.z+b#p.l.m.n    <a id="q" class="x y z"></a><b id="p" class="l m n"></b>

## Tag expansion

    table+                 \n<table>\n<tr>\n<td>\n</td>\n</tr>\n</table>
    dl+                    \n<dl>\n<dt>\n</dt>\n<dd>\n</dd>\n</dl>
    ul+                    \n<ul>\n<li>\n</li>\n</ul>
    ul++ol+                \n<ul>\n<li>\n</li>\n</ul>\n<ol>\n<li>\n</li>\n</ol>
    ul#q.x.y m=l+          \n<ul id="q" class="x y" m="l">\n<li>\n</li>\n</ul>                 

## Parent > child

    a>b                    <a><b></b></a>
    a>b>c                  <a><b><c></c></b></a>
    a.x>b                  <a class="x"><b></b></a>
    a#q.x>b                <a id="q" class="x"><b></b></a>
    a#q.x.y.z>b            <a id="q" class="x y z"><b></b></a>
    a#q.x.y.z>b#p.l.m.n    <a id="q" class="x y z"><b id="p" class="l m n"></b></a>
    #q>.x                  \n<div id="q">\n<div class="x">\n</div>\n</div>
    a>b+c                  <a><b></b><c></c></a>
    a>b+c>d                <a><b></b><c><d></d></c></a>

## Multiplication

    a*1                    <a></a>
    a*2                    <a></a><a></a>
    a/*2                   <a/><a/>
    a*2+b*2                <a></a><a></a><b></b><b></b>
    a*2>b*2                <a><b></b><b></b></a><a><b></b><b></b></a>
    a>b*2                  <a><b></b><b></b></a>
    a#q.x>b#q.x*2          <a id="q" class="x"><b id="q" class="x"></b><b id="q" class="x"></b></a>
    a#q.x>b/#q.x*2         <a id="q" class="x"><b id="q" class="x"/><b id="q" class="x"/></a>

## Properties

    a x                    <a x=""></a>
    a x=                   <a x=""></a>
    a x=""                 <a x=""></a>
    a x=y                  <a x="y"></a>
    a x="y"                <a x="y"></a>
    a x="()"               <a x="()"></a>
    a x m                  <a x="" m=""></a>
    a x= m=""              <a x="" m=""></a>
    a x=y m=l              <a x="y" m="l"></a>
    a/ x=y m=l             <a x="y" m="l"/>
    a#foo x=y m=l          <a id="foo" x="y" m="l"></a>
    a.foo x=y m=l          <a class="foo" x="y" m="l"></a>
    a#foo.bar.mu x=y m=l   <a id="foo" class="bar mu" x="y" m="l"></a>
    a/#foo.bar.mu x=y m=l  <a id="foo" class="bar mu" x="y" m="l"/>
    a x=y+b                <a x="y"></a><b></b>
    a x=y+b x=y            <a x="y"></a><b x="y"></b>
    a x=y>b                <a x="y"><b></b></a>
    a x=y>b x=y            <a x="y"><b x="y"></b></a>
    a x=y>b x=y+c x=y      <a x="y"><b x="y"></b><c x="y"></c></a>

## Parentheses

    (a)                    <a></a>
    (a)+(b)                <a></a><b></b>
    a>(b)                  <a><b></b></a>
    (a>b)>c                <a><b></b></a>
    (a>b)+c                <a><b></b></a><c></c>
    z+(a>b)+c+k            <z></z><a><b></b></a><c></c><k></k>
    (a)*2                  <a></a><a></a>
    ((a)*2)                <a></a><a></a>
    ((a)*2)                <a></a><a></a>
    (a>b)*2                <a><b></b></a><a><b></b></a>
    (a+b)*2                <a></a><b></b><a></a><b></b>
