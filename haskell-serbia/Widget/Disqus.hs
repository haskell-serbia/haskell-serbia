module Widget.Disqus where

import Import
import Text.Julius (rawJS)

-- disqus widget to render comments 
disqusWidget :: Key Tutorial -> WidgetT App IO ()
disqusWidget articleIdentifier = do
   let textId = Number $ entityKey articleIdentifier
   let page_url = String "http://haskell-serbia.com/tutorial/"
   toWidget([hamlet|
                <div #disqus_thread> 
        |])
   toWidget([julius|
                (function() { 
                    var disqus_config = function () {
                        this.page.url = #{page_url}; 
                        this.page.identifier = #{textId};
                    };
                    var d = document, s = d.createElement('script');
                    s.src = 'https://haskell-serbia.disqus.com/embed.js';
                    s.setAttribute('data-timestamp', +new Date());
                    (d.head || d.body).appendChild(s);
                })();

        |])