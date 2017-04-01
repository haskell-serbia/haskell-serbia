module Widget.Header where

import Prelude
import Yesod

-- twitter widget to render follow button
twitterWidget :: MonadWidget m => String -> m ()
twitterWidget twitteruser = do
   toWidgetBody([hamlet| <a href="https://twitter.com/#{twitteruser}" .twitter-follow-button data-show-count="true">Follow @#{twitteruser}
                            <script async src="//platform.twitter.com/widgets.js" charset="utf-8">
                       |])
