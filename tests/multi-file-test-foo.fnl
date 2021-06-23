(local {: error} (require :init))
(require-macros :init-macros)

(fn foo []
  (restart-case (error :error)
    (:r [] :ok)))

{: foo}
