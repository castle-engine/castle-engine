{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "rich_text_demo.html" }
'Using the TCastleLabel.Tags = true parameters allows to render text with HTML tags. We support a small subset of elements from HTML. We don''t try to implement a HTML renderer here!, only to implement some useful elements that are comfortable to use inline in a rich text. This way you can express more interesting text within a single string, within a single TCastleLabel instance.' + LineEnding +
'' + LineEnding +
'This text <b>is bold</b>. This one is even <b>bold and <i>italic and the same time!</i> (again only bold)</b> (again normal). You can go crazy with colors: <font color="#0000ff">blue <font color="#00FF00">green</font> back to blue <b>now blue and bold! <i>and italic too!</i> back to just bold</b> back to just blue</font>.' + LineEnding +
'' + LineEnding +
'Font sizes also work. Text <small>smaller <small>even smaller <small>even more smaller</small> back</small> back</small> back. This is done by &lt;small&gt; element. You can also use &lt;font size="-number"&gt;: small: <font size="-1">one small <font size="-2">two small <font size="-3">three small</font> back</font> back</font>, large: <font size="+1">one large <font size="+2">two large <font size="+3">three large <font size="+4">four large</font></font> back</font> back</font>.' + LineEnding +
'' + LineEnding +
'Note that embedded &lt;small&gt; element makes the text smaller and smaller, but embedded &lt;font size="-1"&gt; does not. This is consistent with HTML (&lt;font size="-1"&gt; is not relative to parent).' + LineEnding +
'' + LineEnding +
'You can use entities &amp;amp;, &amp;lt;, &amp;gt; &amp;apos; &amp;quot; as expected, to escape special HTML/XML sequences: &amp;, &lt;, &gt; &apos; &quot;.' + LineEnding +
'' + LineEnding +
'Note that a newline in text <b>does</b> result in a newline in output.' + LineEnding +
'This is unlike HTML, but is consistent and very comfortable in our case - you often don''t need the rich text, but you still need to indicate newlines.' + LineEnding +
'So a <i>newline in input is a newline in output</i>, regardless if you use rich text or not.' + LineEnding +
'' + LineEnding +
'You can also use HTML paragraph &lt;p&gt; and &lt;br&gt; and &lt;br/&gt; elements.<br>Newline.<br/>Another newline.<p>Paragraph &lt;p&gt; is simply just 2 newlines, so it makes a line-size break. <p>Line wrapping always works Ok, <font color="#00FF00">even if you go crazy with colors and <b><i>bold/italic</i></b></font>. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.' + LineEnding +
''
