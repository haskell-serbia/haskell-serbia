module Widget.Disqus where

import Import

-- disqus widget to render comments 
disqusWidget :: WidgetT App IO ()
disqusWidget  = do
   let page_identifier = 1 :: Int
   let page_url = "http://localhost:3000/tutorial/" :: Text
   toWidget([hamlet|
            <div #disqus-thread> 
        |])
   toWidget([julius|
            var disqus_config = function () {
                this.page.url = "http://localhost:3000/tutorial/"; 
                this.page.identifier = 1;
            };

            (function() { // DON'T EDIT BELOW THIS LINE
                var d = document, s = d.createElement('script');
                s.src = 'https://haskell-serbia.disqus.com/embed.js';
                s.setAttribute('data-timestamp', +new Date());
                (d.head || d.body).appendChild(s);
            })();
        |])