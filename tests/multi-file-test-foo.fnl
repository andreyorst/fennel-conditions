(local {: error} (require :init))
(require-macros :macros)

(fn foo []
  (restart-case (error :error)
    (:r [] :ok)))

{: foo}
