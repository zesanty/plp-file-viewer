{-# LANGUAGE OverloadedStrings #-}

module Layout (
    renderPageBody,
    layout
) where

import Types
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)

renderUpLink :: FilePath -> Text
renderUpLink p
    | p == "."  = ""
    | otherwise = let parent = takeDirectory p
                      url = if parent `elem` [".", ""] then "/browse/." else "/browse/" <> TL.pack parent
                  in "<h3><a href=\"" <> url <> "\">..</a></h3>"

renderItem :: FilePath -> Text -> Item -> Text
renderItem currentPath activeFile item =
    let name = case item of DirItem n -> n; FileItem n -> n
        linkPath = if currentPath == "." then TL.pack name else TL.pack (currentPath </> name)
        (urlPrefix, suffix, isActive) = case item of
            DirItem _  -> ("/browse/", "/", False)
            FileItem n -> ("/exercise/", "", TL.pack n == activeFile)
    in "<li class=\"" <> (if isActive then "active-file" else "") <> "\"><a href=\"" <> urlPrefix <> linkPath <> "\">" <> TL.pack name <> suffix <> "</a></li>"

renderUploadForm :: FilePath -> Text
renderUploadForm p =
    let displayPath = if p == "." then "/" else "/" <> TL.pack p
    in mconcat ["<form id=\"upload-form\" action='/submit' method='post' enctype='multipart/form-data' class='sidebar-upload'>",
                "<h4>Subir a: '", displayPath, "'</h4>",
                "<input type='hidden' name='current_dir' value='", TL.pack p, "'>",
                "<input id=\"file-input\" name='uploaded_file' type='file'>",
                "<span id=\"upload-status\" class=\"upload-status\"></span>",
                "</form>"]

renderContentPane :: FilePath -> Text -> Text
renderContentPane path content =
    let escapeHtml = TL.replace "<" "&lt;" . TL.replace ">" "&gt;"
    in case takeExtension path of
        ".md" -> mconcat [ "<div class=\"content-pane markdown-body\" id=\"content-output\"></div>",
                           "<script id=\"content-source\" type=\"text/plain\">", content, "</script>" ]
        ".pdf" -> "<div class=\"content-pane\"><embed src=\"/serve/" <> TL.pack path <> "\" type=\"application/pdf\" width=\"100%\" height=\"800px\" /></div>"
        ext -> let langClass = if ext == ".hs" then "language-haskell" else "language-plaintext"
               in mconcat ["<div class=\"content-pane\"><pre class=\"file-viewer\"><code class=\"", langClass, "\">", escapeHtml content, "</code></pre></div>"]

renderPageBody :: PageData -> Text
renderPageBody (BrowserPage path items) = mconcat [
    "<h1>PLP: ", if path == "." then "/" else "/" <> TL.pack path, "</h1>",
    renderUpLink path,
    "<ul>", mconcat $ map (renderItem path "") items, "</ul>",
    renderUploadForm path ]
renderPageBody (FilePage path content items) = mconcat [
    "<h1>Viendo: /", TL.pack path, "</h1>",
    "<div class=\"page-grid\">",
        "<div class=\"sidebar\">",
            renderUpLink path,
            "<ul>", mconcat $ map (renderItem (takeDirectory path) (TL.pack $ takeFileName path)) items, "</ul>",
            renderUploadForm (takeDirectory path),
        "</div>",
        renderContentPane path content,
    "</div>" ]


-- Podría usar pandoc para soportar typst.. y hasta ahí --
layout :: Text -> Text -> Text
layout title body = mconcat [
    "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><title>", title, "</title>",
    "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/style.css\"><link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.30.0/themes/prism-okaidia.min.css\">",
    "<script>MathJax = { tex: { inlineMath: [['$', '$'], ['\\(', '\\)']], displayMath: [['$$', '$$'], ['\\[', '\\]']] } };</script>",
    "<script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>",
    "</head><body><div class=\"container\">", body,
    "</div>",
    "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.30.0/components/prism-core.min.js\"></script>",
    "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.30.0/plugins/autoloader/prism-autoloader.min.js\"></script>",
    "<script src=\"https://cdn.jsdelivr.net/npm/marked/marked.min.js\"></script>",
    "<script>",
    "const source = document.getElementById('content-source');",
    "if (source) { document.getElementById('content-output').innerHTML = marked.parse(source.innerHTML); }",
    "</script>",
    "<script>",
    "const uploadForm = document.getElementById('upload-form');",
    "const fileInput = document.getElementById('file-input');",
    "const uploadStatus = document.getElementById('upload-status');",
    "if (fileInput) {",
    "  fileInput.addEventListener('change', function() {",
    "    if (this.files.length > 0) {",
    "      uploadStatus.textContent = 'Subiendo...';",
    "      uploadForm.submit();",
    "    }",
    "  });",
    "}",
    "</script>",
    "</body></html>" ]