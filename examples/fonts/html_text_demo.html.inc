{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "html_text_demo.html" }
'Using the <b>TCastleLabel.Html = true</b> allows to render text with HTML markup. We support a small subset of elements from HTML. We don''t try to implement a full HTML renderer here!, only to implement some useful elements that are comfortable to use inline in a text. This way you can express more interesting text within a single string, within a single TCastleLabel instance.' + LineEnding +
'' + LineEnding +
'This text <b>is bold</b>. This one is even <b>bold and <i>italic at the same time!</i> (again only bold)</b> (again normal). You can go crazy with colors: <font color="#0000ff">blue <font color="#00FF00">green</font> back to blue <b>now blue and bold! <i>and italic too!</i> back to just bold</b> back to just blue</font>.' + LineEnding +
'' + LineEnding +
'Font sizes also work. Text <small>smaller <small>even smaller <small>even more smaller</small> back</small> back</small> back. This is done by &lt;small&gt; element. You can also use &lt;font size="+/-number"&gt; or &lt;font size="1..7"&gt;: small: <font size="-1">one small <font size="-2">two small <font size="-3">three small</font> back</font> back</font>, large: <font size="+1">one large <font size="+2">two large <font size="+3">three large <font size="+4">four large</font></font> back</font> back</font>.' + LineEnding +
'' + LineEnding +
'You can also use percentages to specify font size relative to base size (this is not possible in HTML). Nesting these tags does not make their percentages relative. <font size="50%">Two times smaller than normal <font size="200%">Two times larger.</font></font>' + LineEnding +
'' + LineEnding +
'Note that embedded &lt;small&gt; element makes the text smaller and smaller, but embedded &lt;font size="-1"&gt; does not. This is consistent with HTML (&lt;font size="-1"&gt; is not relative to parent).' + LineEnding +
'' + LineEnding +
'You can use entities &amp;amp;, &amp;lt;, &amp;gt; &amp;apos; &amp;quot; as expected, to escape special HTML/XML sequences: &amp;, &lt;, &gt; &apos; &quot;.' + LineEnding +
'' + LineEnding +
'HTML comments also work (are omitted, zero size): <!-- comment --> <font color="#666666">&lt;-- this is how to write a comment --&gt;</font>.' + LineEnding +
'' + LineEnding +
'Whitespace: we treat whitespace <b>unlike</b> HTML. This means that a newline in text <b>does</b> result in a newline in output. And more spaces in input <b>do</b> make more space (like this:    ) in output. Here''s a newline:' + LineEnding +
'This is unlike HTML, but is consistent and very comfortable in our case - you often don''t need the HTML markup, but you still need to indicate newlines and more spaces. Another newline:' + LineEnding +
'So a <i>newline/space in input is a visible newline/space in output</i>, regardless if you use HTML markup or not.' + LineEnding +
'' + LineEnding +
'You can also use HTML paragraph &lt;p&gt; and &lt;br&gt; and &lt;br/&gt; elements.<br>Newline.<br/>Another newline.<p>Paragraph &lt;p&gt; is simply just 2 newlines, so it makes a line-size break. <p>Line wrapping always works Ok, <font color="#00FF00">even if you go crazy with colors and <b><i>bold/italic</i></b></font>. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.' + LineEnding +
''
