module Widget.Disqus where

import Import

disqusWidget :: Key Tutorial -> WidgetT App IO ()
disqusWidget articleIdentifier = do
   let textId =  String $ toPathPiece articleIdentifier
   let page_url = String "http://haskell-serbia.com/tutorial/"
   let jsLink  = String "https://haskell-serbia.disqus.com/embed.js"
   toWidget([hamlet|
                <div #disqus_thread>
        |])
   toWidget([julius|
                (function() {
                    var disqus_config = function () {
                        this.page.url = '#{page_url}';
                        this.page.identifier = '#{textId}';
                    };
                    var d = document, s = d.createElement('script');
                    s.src = #{jsLink};
                    s.setAttribute('data-timestamp', +new Date());
                    (d.head || d.body).appendChild(s);
                })();

        |])
