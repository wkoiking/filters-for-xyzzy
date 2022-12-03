# インストール方法

まず、下記で実行体をインストール。

~~~
$ stack install
~~~

`C:/Users/wanag/xyzzy/site-lisp/markdown-mode.l`の以下を書き換える。

~~~
(defvar *tabulate-itemization-pipe-table-command* "C:/Users/wanag/AppData/Roaming/local/bin/tabulate-itemization-pipe-table.exe")
(defvar *tabulate-itemization-tab-table-command* "C:/Users/wanag/AppData/Roaming/local/bin/tabulate-itemization-tab-table.exe")
~~~

外部の標準入力->標準出力のフィルタアプリをxyzzyから使う時の例：

~~~
(defun tabulate-itemization-tab-table ()
  (interactive)
  (save-excursion
    (save-restriction
      (if (narrow-selection)
          (filter-buffer *tabulate-itemization-tab-table-command*)
        (plain-error "有効範囲が指定できてないっす")
        ))))

(defun narrow-selection ()
  "セレクション範囲をナローイングする"
  (and (pre-selection-p)
       (narrow-to-region (selection-mark) (selection-point))
       (return-from narrow-selection t))
  nil)
~~~
