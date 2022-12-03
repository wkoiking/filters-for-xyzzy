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