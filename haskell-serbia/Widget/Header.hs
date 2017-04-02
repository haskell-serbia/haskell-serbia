module Widget.Header where

import Import

-- twitter widget to render follow button
twitterWidget :: String -> WidgetT App IO ()
twitterWidget twitteruser = do
   toWidgetBody([hamlet|<div style=margin-top:20px>
                          <a href="https://twitter.com/#{twitteruser}" .twitter-follow-button data-show-count="true">Follow @#{twitteruser}
                          <script async src="//platform.twitter.com/widgets.js" charset="utf-8">
                       |])
