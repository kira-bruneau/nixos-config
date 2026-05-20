(use-package apheleia
  :config
  (setq
   apheleia-formatters
   (append
    '((prettier
       . ("prettierd" "--stdin-filepath" filepath
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-css
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=css"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-html
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=html"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-graphql
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=graphql"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-javascript
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=babel-flow"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-json
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=json"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-json-stringify
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=json-stringify"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-markdown
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=markdown"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-ruby
       . ("prettierd" "--stdin-filepath" filepath
          "--plugin=@prettier/plugin-ruby" "--parser=ruby"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-scss
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=scss"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-svelte
       . ("prettierd" "--stdin-filepath" filepath
          "--plugin=prettier-plugin-svelte" "--parser=svelte"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-typescript
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=typescript"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "=")))
      (prettier-yaml
       . ("prettierd" "--stdin-filepath" filepath
          "--parser=yaml"
          (string-join (apheleia-formatters-js-indent "--use-tabs" "--tab-width") "="))))
    apheleia-formatters)))
