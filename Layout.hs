module Layout (
    renderPageBody,
    layout
) where

import Types ( PageData(..), Item(..), PageAssets(..) )
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)

mathJaxAssets, markedAssets, prismAssets, uploadFormAssets :: PageAssets
mathJaxAssets = PageAssets
    { headTags = [ "<script>MathJax = { tex: { inlineMath: [['$', '$'], ['\\(', '\\)']], displayMath: [['$$', '$$'], ['\\[', '\\]']] } };</script>"
                 , "<script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>" ]
    , bodyTags = [] }

markedAssets = PageAssets
    { headTags = []
    , bodyTags = [ "<script src=\"https://cdn.jsdelivr.net/npm/marked/marked.min.js\"></script>"
                 , "<script>const s = document.getElementById('content-source'); if (s) { document.getElementById('content-output').innerHTML = marked.parse(s.innerHTML); }</script>" ] }

prismAssets = PageAssets
    { headTags = ["<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.30.0/themes/prism-okaidia.min.css\">"]
    , bodyTags = [ "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.30.0/components/prism-core.min.js\"></script>"
                 , "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.30.0/plugins/autoloader/prism-autoloader.min.js\"></script>" ] }

uploadFormAssets = PageAssets
    { headTags = []
    , bodyTags = [ "<script>",
                   "const uf=document.getElementById('upload-form'),fi=document.getElementById('file-input'),us=document.getElementById('upload-status');",
                   "if(fi){fi.addEventListener('change',function(){if(this.files.length>0){us.textContent='Subiendo...';uf.submit();}});}",
                   "</script>" ] }

renderUpLink :: FilePath -> Text
renderUpLink p | p `elem` [".", ""] = ""
               | otherwise = let parent = takeDirectory p
                                 url = if parent `elem` [".", ""] then "/browse/." else "/browse/" <> TL.pack parent
                             in "<h3><a href=\"" <> url <> "\">..</a></h3>"

renderItem :: FilePath -> Text -> Item -> Text
renderItem currentPath activeFile item =
    let nameStr = case item of DirItem n -> n; FileItem n -> n
        fullPathStr = if currentPath == "." then nameStr else currentPath </> nameStr
        linkPath = TL.pack fullPathStr
        nameText = TL.pack nameStr
        (urlPrefix, suffix, isActive) = case item of
            DirItem _  -> ("/browse/", "/", False)
            FileItem n -> ("/exercise/", "", TL.pack n == activeFile)
    in "<li class=\"" <> (if isActive then "active-file" else "") <> "\"><a href=\"" <> urlPrefix <> linkPath <> "\">" <> nameText <> suffix <> "</a></li>"

renderUploadForm :: FilePath -> Text
renderUploadForm p = mconcat
    [ "<form id=\"upload-form\" action='/submit' method='post' enctype='multipart/form-data' class='sidebar-upload'>"
    , "<h4>Subir a: /", TL.pack p, "'</h4>"
    , "<input type='hidden' name='current_dir' value='", TL.pack p, "'>"
    , "<input id=\"file-input\" name='uploaded_file' type='file'>"
    , "<span id=\"upload-status\" class=\"upload-status\"></span>"
    , "</form>" ]

renderSidebar :: FilePath -> Text -> [Item] -> Text
renderSidebar path activeFile items = mconcat
    [ "<div class=\"sidebar\">"
    , renderUpLink path
    , "<ul>", mconcat $ map (renderItem path activeFile) items, "</ul>"
    , renderUploadForm path
    , "</div>" ]

renderMarkdown, renderPdf, renderPlainText :: FilePath -> Text -> (Text, PageAssets)
renderMarkdown _ content =
    ( mconcat [ "<div class=\"content-pane markdown-body\" id=\"content-output\"></div>"
              , "<script id=\"content-source\" type=\"text/plain\">", content, "</script>" ]
    , mconcat [markedAssets, mathJaxAssets, prismAssets] )

renderPdf path _ =
    ( "<div class=\"content-pane\"><embed src=\"/serve/" <> TL.pack path <> "\" type=\"application/pdf\" width=\"100%\" height=\"800px\" /></div>"
    , mempty )

renderPlainText path content =
    let langClass = if takeExtension path == ".hs" then "language-haskell" else "language-plaintext"
        escapeHtml = TL.replace "<" "&lt;" . TL.replace ">" "&gt;"
    in ( mconcat ["<div class=\"content-pane\"><pre class=\"file-viewer\"><code class=\"", langClass, "\">", escapeHtml content, "</code></pre></div>"]
       , prismAssets )

renderContentPane :: FilePath -> Text -> (Text, PageAssets)
renderContentPane path = case takeExtension path of
    ".md"  -> renderMarkdown path
    ".pdf" -> renderPdf path
    _      -> renderPlainText path

renderPageBody :: PageData -> (Text, PageAssets)
renderPageBody (BrowserPage path items) =
    ( mconcat [ "<h1>Archivos: /", TL.pack path, "</h1>"
              , renderUpLink path
              , "<ul>", mconcat $ map (renderItem path "") items, "</ul>"
              , renderUploadForm path ]
    , mempty )
renderPageBody (FilePage path content items) =
    let (contentPane, assets) = renderContentPane path content
        dir = takeDirectory path
        filename = TL.pack $ takeFileName path
        sidebar = renderSidebar dir filename items
        body = mconcat [ "<h1>Viendo: /", TL.pack path, "</h1>"
                       , "<div class=\"page-grid\">", sidebar, contentPane, "</div>" ]
    in (body, assets)

layout :: Text -> PageAssets -> Text -> Text
layout title pageAssets body =
    let finalAssets = pageAssets <> uploadFormAssets
    in mconcat
    [ "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><title>", title, "</title>"
    , "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/style.css\">"
    , mconcat (headTags finalAssets)
    , "</head><body><div class=\"container\">", body, "</div>"
    , mconcat (bodyTags finalAssets)
    , "</body></html>"
    ]