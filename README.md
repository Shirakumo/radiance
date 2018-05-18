![image](https://github.com/Shirakumo/radiance/blob/master/static/radiance.png?raw=true)

## Radianceについて

Radianceは、Webアプリケーションの開発環境です。Webフレームワークのようなものですが、より汎用的で、変更も簡単です。 特別な変更を加える必要なく、個人的なWebサイトやWebアプリを、簡単に書くことができます。

## 入手するには

Radianceと関連するモジュールやアプリケーションは、Quicklispを通して配布されています。Radianceをインストールするには次のようにしてください:

```lisp
(ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
(ql:quickload :radiance)
```

この後では、Quicklispの`quickload`コマンドで、[Purplish](https://github.com/Shirakumo/purplish)等のRadianceモジュールを読み込んで利用できるようになります。

## 利用法

Radianceのチュートリアルは[こちら](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%200.md)です。Radianceの重要なコンセプトや、Webアプリを書く方法を紹介しています。このチュートリアルでは、Radianceの大まかな使い方に親しみ、ある特定の機能が必要なとき、どこを調べるべきかの道しるべになるでしょう。チュートリアルの最後では、本番環境での、Radianceのインストールとデプロイ方法について説明します。

## 簡単な例

おそらく、あなたが一番したいことは、HTMLをサーブすることでしょう。では、その方向に進めながら、少しずつ拡張していきましょう。はじめる前に、Radianceの準備をしましょう。

```common-lisp
(ql:quickload :radiance)
(radiance:startup)
```

Radianceを使うのが初めての場合は、`r-welcome`モジュールを使いながら進めましょう。`r-welcome`モジュールを使うと、ブラウザーで開くことができる最初のページへのリンクをつけることができます。まずは、小さなページをリンクづけましょう。

```common-lisp
(in-package :rad-user)

(define-page example "/example" ()
  (setf (content-type *response*) "text/plain") 
  "Hi!")
```

[localhost:8080/example](http://localhost:8080/example)にアクセスすると、"Hi"と表示されるはずです。
これでは、かなり退屈ですね。
代わりにHTMLを出してみましょう。
まずは、[cl-who](http://weitz.de/cl-who/)を使います。

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page"))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

普通であれば、コードを書き直しながら、フォントの書式を変えたり、CSSファイルを追加したりしながら、スタイルを作っていきます。
確かにCSSファイルをサーブすることは可能ですが、長い目でみれば、それは最適の方法とはいえません。

代わりに、モジュール(module)を作る方法を紹介します。
モジュールを使うことで、Webページを適切に整理することができます。
モジュール(module)のためのファイルは、手動で作ることができますが、ここではRadianceの雛形を用いて、モジュールを自動生成しましょう。

```common-lisp
(create-module "example")
```

モジュールを自動生成すると、モジュールがあるパスを返します。
生成されるものは、ASDFのシステムファイル、mainのlispファイル、`static`と`template`という名前のフォルダ２つです。

`static`フォルダには静的にサーブされるファイルが入り、`template`にはテンプレートエンジン関連のファイルが入ります。

`example.lisp`を開いて、先ほどの例で使ったコードを引き継いでページを定義しましょう。

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page"))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```
ページは、シンボル名で特定されます。
自分のモジュールを作ることでパッケージを持つことになります。
先ほどの例のシンボルは、以前に一度も使われていないものです。
名前の衝突を避けるために、`rad-user`パッケージでページを消す必要があるかもしれません。

```common-lisp
(remove-page 'rad-user::example)
```

次に、簡単なCSSファイルを作りましょう。
`static`フォルダに、`example.css`という名前で配置しましょう。
自分で書くのが面倒であれば、次のCSSコードを使ってください。

```CSS
body{
    font-family: sans-serif;
    font-size: 12pt;
    background: #EEE;
}

header{
    text-align: center;
}

main{
    width: 800px;
    margin: 0 auto 0 auto;
    background: #FFF;
    padding: 10px;
    border: 1px solid #BBB;
    border-radius: 5px;
}
```
では、CSSファイルにリンクするように、HTMLを修正しましょう。
アドレスがスタイルシートにたどり着くために、後に、Radianceのルーティングシステムを使う必要が出てきます。
今はその必要はないので、あせらないでください。

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page")
             (:link :rel "stylesheet" :type "text/css" 
                    :href (uri-to-url "/static/example/example.css" :representation :external)))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

ページを再読み込みすると、スタイルが適用されているはずです。
`uri-to-url`が動作する原理について詳しく知りたくなると思いますが、それは後ほど説明します。
要は、どのようにセットアップしても、静的なファイルへのリンクは、適切に解決されるということです。

## 1. Radianceのコンセプトと部品
### 1.1 URI

Radianceの中心的な概念は`URI`です。
URIはオブジェクトであり、ドメイン、ポート番号(オプション)、パスを含むリストで構成されます。

RadianceのURIは、一般的なURIから要素を抽出したものなので、スキーマやクエリ、フラグメント等は含みません。
また、重要な違いとしては、`domains`のURIは、フレームワーク全体の複数のポイントで使われることであり、
`location`を捕捉するときや、`dispatch matching`を処理するためにも使われます。

URIは、変更可能であり。URIの修正は、[クリティカルパス](https://www.weblio.jp/content/critical+path)がある場所で行われるはずなので、パフォーマンス上重要です。予想されない方法でURIを修正した場合、予期しない動作につながる可能性があります。

URIは、単一の文字列で表現されて、文字列にシリアライズされて、完全なURIのオブジェクトにパースして戻すことも可能です。
URIは、FASLファイルにリテラルとして書き出されるので、マクロから吐き出しても大丈夫です。
URIのシンタックスは、次の通りです。

```BNF
URI     ::= DOMAINS? (':' PORT)? '/' PATH?
DOMAINS ::= DOMAIN ('.' DOMAIN)*
DOMAIN  ::= ('a'..'Z' | '0'..'9' | '-')
PORT    ::= ('0'..'9'){1, 5}
PATH    ::= .*
```

URIをURLに変換するには、`uri-to-url`が使えます。`uri-to-url`を使うと、反転(reversal)、エンコーディング(encoding)、体裁直しは自動で行われます。

`uri`, `domains`, `port`, `path`, `matcher`, `uri-string`, `make-uri`, `make-url`, `ensure-uri`, `copy-uri`, `parse-uri`, `uri<`, `uri>`, `uri=`, `uri-matches`, `merge-uris`, `represent-uri`, `uri-to-url`を参考にしてください。

### 1.2 リクエストとレスポンス

行き来するデータを格納しておくために、`request`オブジェクトと`response`オブジェクトを作ることにしました。
`request`オブジェクトは、リクエストがどこに向かうのかを合わすURI、また、POST、GET、ヘッダ、クッキーなどのHTTP通信のペイロードデータを全て保持します。
`response`オブジェクトは、リターンコード、ヘッダー、クッキー、実際のBODYデータを含みます。

リクエストが行われている間、これらの2つのオブジェクトが必ず存在し、`*request*`と`*response*`に束縛されなければいけません。
それらは、動的なページを生成するために必要な重要情報を多く格納しています。
さらに、リクエストは`data`テーブルを含んでおり、任意のデータを保持することができます。
これは、システム内の各々の部品の間で、リクエストの実行中に取得されるような情報をやりとりをするときに役立ちます。

リクエストは、必ずしもHTTPサーバからくる必要はありません。
動作をテストするためには、プログラムからリクエストを送ることも可能です。
どのような場合であっても、リクエストをディスパッチするインターフェイスは、`request`と呼ばれます。
この仕組みは、リクエストとレスポンスを構築して、URIを適切に処理します。
もし自分自身でリクエストオブジェクトを送りたいのであれば、`execute-request`を使うこともできます。

実際にリクエストを処理する方法に関する詳しい情報は、dispatcher、pages、API endpointをご参照ください。

`*request*`, `*response*`, `*default-external-format*`, `*default-content-type*  `, `request`, `uri`, `http-method`, `headers`, `post-data`, `get-data`, `cookies`, `user-agent`, `referer`, `domain`, `remote`, `data`, `issue-time`, `response`, `data`, `return-code`, `content-type`, `external-format`, `headers`, `cookies`, `cookie`, `name`, `value`, `domain`, `path`, `expires`, `http-only`, `secure`, `cookie-header`, `cookie`, `get-var`, `post-var`, `post/get`, `header`, `file`, `redirect`, `serve-file`, `request-run-time`, `*debugger*`, `handle-condition`, `render-error-page`, `execute-request`, `set-data`, `request`も参考にしてしてください。

### 1.3 ルーティング
リクエストがディスパッチされる前には、ルーティングシステムを通過します。
他のフレームワークでは'routes'はどのハンドラがリクエストを処理するかを指定しますが、Radianceではその方式とは違います。
Radianceにおいてルート(`route`)とは、URI変換の様態です。
システムのこの部分は、２つの**世界**を作成して保持します。**内部の世界**と**外部の世界**です。

**内部の世界**は、実際にWebアプリケーションが住む世界です。
**外部の世界**は、HTTPサーバとWebサイトを利用するユーザが住む世界です。
この区別は、あるサーバにおける潜在的な罠を避けてWebアプロケーションを書くために、必要不可欠です。
あなたのアプリケーションを動作させるために、どのようなドメインやポート、パスが必要になるか、心配する必要がありません。
一方で、Webの管理者として、アプリが壊れないように、システムをあなたの望み通りにカスタマイズして動作させる必要があります。

そのために、ルーティングが役に立ちます。ルーティングには、`Mapping`と`Reversal`の2種類があります:
Mappingは、外部の世界からくるURIを内部の世界のURIに変換します。
普通は、トップレベルのドメインを切り取り、サブドメインにマッピングします。
Reversalは逆のことをします。内部の世界から外部の世界へといきます。
このことは、あなたが提供するWebページが、実際に外部からアクセス可能なリソースを参照できるようにするために、欠かせません。

ルーティングは、任意の処理を行うことができます。
基本的なレベルでは、何らかの方法でURIを修正する関数です。
この利用法では、管理者として、あなたの期待に十分応える強力で柔軟なシステムを構築することができます。
アプリケーションの開発者として、`external-uri`か`uri-to-url`を、ページ内に置く全てのリンクに使うようにしてください。

`route`, `name`, `direction`, `priority`, `translator`, `route`, `remove-route`, `list-routes`, `define-route`, `define-matching-route`, `define-target-route`, `define-string-route`, `internal-uri`, `external-uri`も参考にしてください。

### 1.4 URIディスパッチャー
ついに、リクエストに対して、実際にコンテンツを生成する段階まできました。
URIディスパッチャーはURIのサブクラスであり、名前、関数、優先順位も運びます。

優先順位に基づいたリストは、いつリクエストがきても実行されます。
リクエストのURIは、それぞれのディスパッチャーに対応して、最初に対応する１番目のディスパッチャーの関数が実行されます。
たったこれだけです。

ディスパッチャーの関数は、ページの内容を提供するために、必要な値をレスポンスのオブジェクトに設定する責任があります。
そのためには、レスポンスのオブジェクトの`data`のフィールドに直接設定するか、関数から適切な値を返します。
Radianceは、4つの型のデータ（`stream`、`pathname`、`string`、`(array (unsigned-byte 8))`）を受け取ります。

もしURIディスパッチャーが明示的な優先順位の番号を持っていない場合は、
優先順位はURIの特異性によって決まります。
どのように計算がされるかについて詳しく知りたい場合は、URIソーティング関数である`uri>`をみてください。

`uri-dispatcher`, `name`, `dispatch-function`, `priority`, `uri-dispatcher`, `remove-uri-dispatcher`, `list-uri-dispatchers`, `uri-dispatcher>`, `define-uri-dispatcher`, `dispatch`もご参照ください。

### 1.5 Page

ページは、実際にコンテンツを提供する関数を定義するために用いるものです。
しかし、ページは単なるuriディスパッチャーであり、物事を簡単にするためのマクロをいくつか含んでいるものです。
注目してもらいたいのは、拡張できるオプションです。詳しくみていきましょう。

Radianceによってデフォルトでセットアップされるページはいくつかあります。
`favicon`と`robots`のページは、Radianceの`static/`ディレクトリでサーブされます。
Production段階のサーバでも、自分のサイトにもファイルを提供したり更新したりしたいはずです。

そのような目的を満たすために、`static`ページの仕組みがあります。`static`ページは、静的なコンテンツをWebアプリケーションとモジュールにサーブします。
`static`ページは、そのドメインでも`/static/...`のパスで有効であり、最初のディレクトリがモジュールの名前である形式である必要があります。
残りは、モジュールの`static/`ディレクトリの範囲にあるパスです。
この仕組みにより、CSS、JavaScript、画像などの静的なファイルを参照することができます。

最後に、`api`ページですが、APIエンドポイントのディスパッチを処理する役割があります。
これについては次の章で説明します。
ページは、静的ファイルの場合と同様に、どのドメインにおいても`/api/...`のパスで捕捉することで、同じように動作します。

`page`, `remove-page`, `define-page`をご参照ください。

### 1.6 APIエンドポイント

Radianceは、REST APIとの連携もサポートしています。
これは取ってつけたような機能ではありません。
多くの現代のアプリケーションは、そのようなAPIを提供することが多く、Radianceは、APIエンドポイントを含むアプリケーションを書く方法を提案します。

コンセプトとしては、APIエンドポイントとは、ブラウザーのリクエストに応じて呼び出される関数です。
そのレスポンスは、リクエスト主が読める形式にシリアライズされます。
重要な点は、APIエンドポイントは、ユーザからも、プログラムからも利用可能であるべきということです。
APIを通してプログラムから実行されるアクションは全て、ユーザによって実行されるものであるので、Radianceでは、両方から利用可能にするよう推奨しています。
重複を避けるために、これら2つは、1つとして扱われます。

どのようなデータ修正にアクションも、APIエンドポイントを通して提供されます。ユーザかプログラムのどちらがリクエストをしたのかはあまり違いなく扱われます。
ユーザがリクエストを行う場合gは、適切なページにリダイレクトを行います。
プログラムからリクエストが行われた場合は、読み込めるフォーマットで、データのペイロードが提供されます。

全てのパートのうち、APIフォーマットのシステムですが、データを特別なフォーマットにシリアライズすることを担当します。
デフォルトでは、S式を基本とするフォーマットが提供されていますが、JSON形式の出力も簡単にロードできます。

次に、`browser`のPOST/GETパラメータの仕様をみましょう。
そのパラメータが`"true"`という文字列を含む場合、APIリクエストはユーザからであると扱われて、データのペイロードはされずにリダイレクトされます。

あなたのアプロケーションは、統一されたAPIを提供するために、これらをうまく使う必要があります。
今、実際のエンドポイントの定義は、名前、関数、引数を記述するラムダリスト、リクエストのパーズ関数で構成されます。
引数の典型としては、必須の引数かオプショナル引数が理にかなっています。
結局、HTTPリクエストは、**キーワード引数**しかもつことができません。**キーワード引数**は、あってもなくてもいいです。

APIエンドポイントの名前は、どこにリーチできるかを示す識別名として機能します。
APIエンドポイントは、`/api/`パスに存在し、エンドポイントの名前もそれに従います。
エンドポイントを修正する際には、あなたのモジュールやアプリケーションが、他のエンドポイントをうっかり踏み外さないように気をつけなければいけません。
これはURIディスパッチのときとは違いますが、理由は、APIエンドポイントは、曖昧さやパスのプロセスを許可しないからです。
なので、全てのエンドポイントは、唯一のパスをもなければいけません。唯一のパスは、直接、名前としてサーブします。

生の(raw)関数は、APIがインターフェイスを提供する関数です。
生の関数は、リクエストされたアクションを行い、適切なデータを上で述べた通りに返す役割を果たします。
フォーマットされたAPIのデータを返すためには、`api-output`を使います。
ブラウザリクエストからリクエストを受けてリダイレクトをするには、`redirect`を使います。

最後に、リクエストのパーズ関数は、リクエストオブジェクトを受け取り、関数が必要な実引数を抽出して、最終的に、可能であれば、適切な引数を適用させて関数を呼び出します。
パーズ関数は、もし必要な引数が見当たらない場合、`api-argument-missing`エラーの信号を送ります。
無駄な実引数は無視されます。

`call-api`を用いると、プログラムからAPIエンドポイントを呼び出すこともできます。`call-api-request`を使うと、Requestをシュミレーションすることができます。
どちらもURIディスパッチの仕組みを通る必要はありません。

ページと似ていますが、APIエンドポイントの定義は、定義を簡単にするために、拡張オプションも受け付けます。
オプションについては、次で説明します。

`api`, `*default-api-format*`, `*serialize-fallback*`, `api-format`, `remove-api-format`, `list-api-formats`, `define-api-format`, `api-output`, `api-serialize`, `api-endpoint`, `remove-api-endpoint`, `list-api-endpoints`, `api-endpoint`, `name`, `handler`, `argslist`, `request-handler`, `call-api-request`, `call-api`, `define-api`をご参照ください。

### 1.7 オプション

オプションは、拡張可能な定義のマクロを提供します。
これは、フレームワークが何か定義する際に、共通のオペレーションをより短しようと拡張しようとするときに役に立ちます。
例えば、アクセス権があるユーザに対して、ページかAPIエンドポイントを制限するような共通するタスクがあるとします。

このような実装を簡単にするために、Radianceは一般的なオプションに仕組みを提供します。
オプションは、定義のマクロが属するオプションの型によって分けられます。
Radianceは、`api`と`page`のオプションを提供します。

それぞれのオプションは、オプションの型に応じて、名前と多くの引数を受け入れる関数のために、キーワードをもっています。
定義名、ボディー部、最終にオプションに渡される値を含んだリストが、いつも引数として与えられます。

この拡張用(expansion)の関数は、定義マクロのボディーの式を変換します。
環境を設定を許可するために、定義自体では含まれない式を吐き出すこともできます。

`option`, `option-type`, `name`, `expander`, `option`, `remove-option`, `list-options`, `define-option`, `expand-options`をご参照ください。

### 1.8 モジュール
モジュールの概念は、Radianceにおいて必要不可欠です。
全体をこうせいする**部品**として働きます。
技術レベルでいうと、モジュールは、特別なメタデータが与えられたパッケージです。
モジュールは、`modularize`しステmyによって提供され、フック、トリガー、インターフェイス等を使いやすくして、他の情報をトラッキングするために使われます。

`defpackage`を使う代わりに、`define-module`を使うようにしてください。
シンタックスは`defpackage`ですが、`:domain`のような特別なオプションを含んでいるので、
モジュールが機能するプライマリ・ドメインを特定することができます。

モジュールのシステムでは、ASDFのシステムをモジュールに記すこともできます。
もし記せば、そのASDFシステムは**仮想のモジュール**になります。
このようにするためには、システムの定義に3つのオプションを追加する必要があります:

```commonlisp
:defsystem-depends-on (:radiance)
:class "radiance:virtual-module"
:module-name "MY-MODULE"
```
こうすることで、Radianceは、ASDFのシステム情報を特定して、あなたのモジュールに関連づけることができます。
新しいモジュールのために、必須のシステムとモジュールの定義を自動で行うには、`create-module`をみてください。

`virtual-module`, `virtual-module-name`, `define-module`, `define-module-extension`, `delete-module`, `module`, `module-p`, `module-storage`, `module-storage-remove`, `module-identifier`, `module-name`, `current-module`, `module-domain`, `module-permissions`, `module-dependencies`, `module-required-interfaces`, `module-required-systems`, `module-pages`, `module-api-endpoints`, `describe-module`, `find-modules-directory`, `*modules-directory*`, `create-module`をご参照ください。

### 1.9 フック

Radianceには、互いのモジュールを互いに連携させるために、フック(hook)の仕組みがあります。
フックを使うと、ある種のイベントに反応して、任意の関数を実行することができます。
例えば、意見交換をするようなソフトウェアでは、新しい投稿が作成されるごとにトリガーされるフックを設定することがでいます。
拡張では、追加のタスクを実行するためにトリガーをフックに対して定義できます。

フックは、任意の数のトリガーをもつことができますが、トリガーは長すぎないようにしてください。
フックをトリガーすることは、全てのトリガーが終了するまでブロックされる動作だからです。
そのようなわけで、長い間続くようなトリガーの実行は、リクエストへのレスポンスを遅らせてしまう可能性があります。 long.

フックは、長い間は**on**であり、後に**off**になるスイッチにような関数であるべきです。
もし新しいトリガーが実行中に定義された場合は、自動で呼び出されるべきです。
これは、`define-hook-switch`が容易にすることです。
`define-hook-switch`は2つのフックを作ります。1つ目がトリガーされると、それに定義されているトリガーは、後に2つ目のフックがトリガーされたときに、自動で呼び出されます。
このおかげで、仮にトリガーがサーバの起動後に定義されたとしても、`server-start`が適切に動作します。

`list-hooks`, `define-hook`, `remove-hook`, `define-trigger`, `remove-trigger`, `trigger`, `define-hook-switch`をご参照ください。

### 1.10 インターフェイス
システムが一枚岩になることを避けるために、バックエンドは拡張できるようにするために、
Radianceはインターフェイスの仕組みを含んでいます。
一般的な意味では、インターフェイスとは、関数やマクロ、変数がどのように動作するかについて、約束事を決めますが、実際に実装するのはインターフェイスではありません。
インターフェイスが動作する全てを作っている実際の機能性は、実装の外側にあります。
このことにより、ユーザはインターフェイスに対してコードを書くことができ、特定のバックエンドに結びつけることなく、与えられた機能を利用することができます。

For a concrete example, let's say there's an interface for a database. 
This is sensible, since there are many different kinds of databases, 
that all offer many differing ways of interaction, 
but still all also offer some very common operations: storing data, retrieving data, and modifying the data. 
Thus we create an interface that offers these common operations. 
It is then up to an implementation for a specific kind of database to make the actual operations work. 
As an application writer, you can then make use of the database interface, 
and with it, make your application automatically work with lots of different databases.

Aside from giving application writers an advantage, 
the decoupling that the interfaces provide also mean that a system administrator can write their own implementation with relative ease, should their particular requirements not be met by existing implementations. 
Thanks to the opaqueness of the interfaces, an implementation can both provide a bridge to something that runs in the lisp process, and something that is completely external. 
This leaves a lot of choice open for the administrator of a production system to allow them to pick exactly what they need.

In practise, interfaces are special kinds of modules, 
and thus special kinds of packages. 
As part of their definition, they include a series of definitions for other bindings like functions, variables, etc. 
Since it is a package, as a user you can use the interface's components just like you would use anything else in any other package. 
There is no difference. 
As an implementation writer, you then simply redefine all the definitions that the interface outlines.

In order to actually load a module that makes use of an interface, an implementation for the interface has to be loaded beforehand. 
Otherwise, macros could not work properly. 
Thus, in order to allow depending on interfaces in your ASDF system definition without having to refer to a specific implementation, Radiance provides an ASDF extension. 
This extension makes it possible to add a list like `(:interface :foo)` to your `:depends-on` list. 
Radiance will then resolve the interface to a concrete implementation thereof when the module is loaded.

Radiance provides a bunch of standard interfaces. 
Each of those interfaces has at least one standard implementation provided by [radiance-contribs](https://shirakumo.org/projects/radiance-contribs). 
The interfaces are:

* `admin`  
  拡張可能な管理者ページを提供します。
* `auth`  
  認証とログインに関する全てを扱います。
* `ban`  
  IPアドレスによってユーザがサイトにアクセスすることを禁止します。
* `cache`  
  キャッシュのための仕組みを提供します。
* `database`  
  柔軟なデータベースのインターフェイスです。オブジェクト保存、リレーショナルデータベースをバックエンドとして利用できます。
* `logger`
  ログ出力のためのインターフェイスです。デバッグやメッセージを出力できます。
* `mail`  
  メールを送るための最小限のインターフェイスです。

* `profile`  
  ユーザのプロフィールや属性を拡張するために使います。

* `rate`  
  特定のリソースにrate limitationを許可します。

* `server`  
  Rdianceを外部の世界と結びつける架け橋の役割を果たすインターフェイスです。

* `session`  
  トラッキングするために、持続するセッションを保証します。

* `user`  
  ユーザアカウントとパーミションの機能を提供します。

それぞれのインターフェイスについては、次の章で説明します。

`interface`, `interface-p`, `implementation`, `implements`, `reset-interface`, `define-interface-extension`, `find-implementation`, `load-implementation`, `define-interface`, `define-implement-trigger`をご参照ください。

### 1.11 環境

動作中の複数のRadianceインスタンスが同じマシンんで異なる設定できるように、Radianceでは環境(Environment)という仕組みを用意しています。
環境とは、基本的には、Radianceとロードされるモジュールのための設定ファイルの一式です。
Radianceの設定は、インターフェイスを選択される実装にマッピングすることで、もしインターフェイスが求められたときに選択されるように決定します。

`startup`が呼び出された時、どれだけ遅くとも、特定の環境が選択されます。早ければ、モジュールがロードされたときに選択されます。
後者の場合は、環境を選ぶために、インタラクティブな再起動が可能です。
これは必須の機能ですが、理由は、そうでなければ、Radianceがインターフェイスのマッピングを解決できないからです。

環境のシステムの一部として、Radianceは、あなたのアプリケーションで使える(おそらく、使うべき)設定システムを提供します。
設定システムを使うと、それぞれの環境ごとに、適切に設定することができます。
その設定は、いつでも持続性があるうえ、可読性にも優れたフォーマットで保存されるので、特別なツールで読み込んだり、修正したりする必要はありません。

実際に設定手順でどのように保存処理がされているかを知りたい方は、[ubiquitous](https://shinmera.github.io/ubiquitous)を参考にしてください。 
`value`関数の代わりに、Radianceでは`config`関数を使えます。

`environment-change`, `environment`, `check-environment`, `mconfig-pathname`, `mconfig-storage`, `mconfig`, `defaulted-mconfig`, `config`, `defaulted-config`をご参照ください。

### 1.12 インスタンスの管理
最後に、Radianceは、起動からシャットダウンまでのシーケンスを提供しています。このおかげで、ソフトは適切に起動して利用可能になり、その後、綺麗に片付けて終了することを確かにします。

そのシーケンスの大部分は、正しい順番で、適切な回数、特定のフックが呼び出されることによって実現されいます。

インターフェイスの関数を適切に使うことで、サーバを手動で起動することは可能ですが、その方法で、アプリケーションが正しく動作すると想定するのはやめてください。
多くは、特定のフックが適切な順番で呼び出されることを要求しています。
このような理由で、`startup`と`shutdown`でRadianceインスタンスを管理する必要があります。
`startup`と`shutdown`のドキュメントには、どのフックが、どの順番で呼び出されているかが説明されています。
実装では、シンボルがexportされていない限り、追加で特定されない定義をインターフェオスのパッケージのシンボルに加えることができます。

`*startup-time*`, `uptime`, `server-start`, `server-ready`, `server-stop`, `server-shutdown`, `startup`, `startup-done`, `shutdown`, `shutdown-done`, `started-p`をご参照ください。

## 2. 標準のインターフェイス

インターフェイスは、Radianceとcore packageと一緒に配布されています。
ライブラリは、追加のインターフェイスを提供することも可能です。
インターフェイスの実装ですが、インターフェイス定義では、次の制限の緩和が許可されています:

`&key`引数を含むラムダリストは、実装依存のキーワード引数を使って拡張できます。
`&optional`引数を含み、`&key`か`&rest`を含まないラムダリストは、オプショナル引数で拡張できます。
必須の引数しか含まないラムダリストは、オプショナル引数かキーワード引数で拡張できます。

### 2.1 管理(admin)

このインターフェイスを使うと、管理者ページを作成できます。
ユーザ構成の設定やシステム情報の表示のために使えます。
"管理(administration)"という名前ですが、システムの管理者だけに限ったものではありません。
どのユーザにも利用できることができます。

管理者ページは、カテゴリー分けされたメニュやパネルを表示できなければいけません。
パネル群は、他のモジュールによって提供されており、`admin:define-panel`で追加できます。
秘密情報を含むようなパネルにアクセスするパネルは、`:access`オプションでアクセスを禁止させるようにしてください。

パーミッションについては、 userインターフェイスを参照してください。

管理者ページや特定のパネルにリンクをはるには、`page`リソースを使ってください。

`admin:list-panels`, `admin:remove-panel`, `admin:define-panel`, `admin:panel`をご覧ください。

### 2.2 auth

`認証インターフェイス`は、ユーザをリクエストと結びつけます。
そのために、ユーザがシステムに対して自分自身を認証させる方法を提供しなければいけません。
どのように実現するかは、実装次第です。
実装は、認証のプロセスが初期化するためのページを提供しなければいけません。
`page`のソースを通してURIを認証プロセスに渡して、`"login"`を引数として渡します。

現在リクエストに結び付けられているユーザを`auth:current`コマンドで調べることができます。
ユーザが`"anonymous"`と解釈される場合は、`NIL`を返します。
詳しくはuserインターフェイスを参照ください。

`auth:*login-timeout*`, `auth:page`, `auth:current`, `auth:associate`もご覧ください。

### 2.3 ban
`banインターフェイス`を使うと、IP BANができます。
IP BANされたクライアントのIPアドレスからは、リクエストするページに対してアクセス出来なくなります。
BANは、タイムアウトの後、手動・自動いずれでも、離す(lift)することができます。
実装としては、ユーザのIPを監視するために追加で労力を割くするようなことは想定していません。

`ban:jail`, `ban:list`, `ban:jail-time`, `ban:release`をご参照ください。

### 2.4 キャッシュ
キャッシュインターフェイスは、一般的なキャッシュの仕組みを提供します。カスタマイズ可能で無効化のテストをもっています。
キャッシュを明示的に新しくするには、`cache:renew`をつかいます。
`cache:with-cache`を使うと、キャッシュの部品を構築することができ、テストのformがtrueと評価されたときに、BODYのキャッシュ値が返されるようにできます。

キャッシュ値が保存される方法は実装によります。 `cache:get`か`cache:with-cache`を使うと、キャッシュ値は文字列かバイト列に強制変換されます。
実装では、キャッシュに格納できる　typeには制限がありませんが、少なくとも、文字列とバイト列はサポートします。

キャッシュ値の名前は、名前とパッケージ名が、次の文字を含まないシンボルでなければいけません:`<>:"/\|?*.` 
キャッシュ値の変形は、`princ`によって表示される表現とは区別されるオブジェクトである必要があります。
その際には、先ほどと同じ文字の制限が適用されます。

`cache:get`, `cache:renew`, `cache:with-cache`をご参照ください。

### 2.5 データベース
このインターフェイスは、データを持続させるためのレイヤーを提供します。通常は、データベースと呼ばれるレイヤーです。
リレーショナル型のデータベースである必要はありませんが、そうであってもいいです。
実装の変数を保持するために、基本的なデータベースの機能しかサポートされません。(joinsやtriggers等はありません)
データ型も、整数、float、文字列に限定されます。
これらの制限にも関わらず、多くのアプリケーションにおいて、データベースインターフェイスはとても役に立ちます。

伝統的なRDMBの用語と区別するために、特別な用語が使われます:
`schema`は"structure"、`table`は"collection"、`row`は"record"とします。

データベースに接続する前に、データベース関連の命令を実行すると、未定義の動作を招きます。
Radianceでは、Radianceが動作している間はデータベースが接続されることを保障しているので、どのページ、どのAPI、どのURIディスパッチャーの定義において、データベースインターフェイスを問題なく使えます。

実際にデータ保存を行うための関数は、`db:insert`、`db:remove`、`db:update`、`db:select`、`db:iterate`です。
それらは、あなたが期待するように動作するはずです。

詳しくは、それぞれの関数に書かれているコメントを読んでください。
コレクションの作り方、どのような制限があるかを知りたい方は、`db:create`のコメントも参考にしてください。

データベースは、データ操作が完了すれば、Radianceを再起動したり、Lispイメージ、マシンがクラッシュしたとしても、、データの変更は存続されなければいけません。

`database:condition`, `database:connection-failed`, `database:connection-already-open`, `database:collection-condition`, `database:invalid-collection`, `database:collection-already-exists`, `database:invalid-field`, `database:id`, `database:ensure-id`, `database:connect`, `database:disconnect`, `database:connected-p`, `database:collections`, `database:collection-exists-p`, `database:create`, `database:structure`, `database:empty`, `database:drop`, `database:iterate`, `database:select`, `database:count`, `database:insert`, `database:remove`, `database:update`, `database:with-transaction`, `database:query`, `database:connected`, `database:disconnected`を参照してください。

### 2.6 logger

ログの関数を提供します。システムの中で関連して起きていることについて、ログのメッセージを出すことができます。
ログの出力内容と出力方法に関しては、実装とシステムの管理者次第です。

`logger:log`, `logger:trace`, `logger:debug`, `logger:info`, `logger:warn`, `logger:error`, `logger:severe`, `logger:fatal`をご覧ください。

### 2.7 メール
メールを送る仕組みを組み込むことができます。
様々なコンポーネントが、Webサイトの外からユーザとつながるために、メールのアクセスが必要になるかもしれません。
リモートサーバ、ローカル環境でのメール送信等、メールの送信方法の設定は、処理系依存です。
`mail:send`のフックを使うと、メールが送られる前に、メールに反応することができます。

`mail:send`をご覧ください。

### 2.8 プロフィール
プロフィールインターフェイスは、userにある種のpresenceをもたせたいアプリケーションにおいて、共通で使用されるuserインターフェイスを拡張できるようにします。
そのインターフェイスは、機能の一部として、ユーザの**プロフィール**が表示されるページを提供する必要があります。
その**プロフィール**は、数種類のパネルを表示しなければいけません。
パネルは、他のモジュールによって提供されており、`profile:define-panel`で追加できます。

`page`のリソースの型を通して、URIをプロファイルのページに移動することができます。

そのインターフェイスは、視覚的にユーザを特定させるために`profile:avatar`で**アバター画像**にアクセスさせることもできます。
また、`profile:name`を使うと、ユーザがユーザ名をカスタマイズできます。
さらに、`profile:fields`、`profile:add-field`、`profile:remove-field`を使うと、どのようなデータをユーザの属性に含むか、それを公(public)に表示させるかどうかを指定できます。

`profile:page`, `profile:avatar`, `profile:name`, `profile:fields`, `profile:add-field`, `profile:remove-field`, `profile:list-panels`, `profile:remove-panel`, `profile:define-panel`, `profile:panel`をご参照ください。

### 2.9 rate

[Rate limitation](https://en.wikipedia.org/wiki/Rate_limiting)の仕組みを提供します。秘密情報やコストが高いリソースへの負荷の高いアクセスを防ぐことができます。
２つの段階があります。第1段階は、`rate:define-limit`により、特定のリソースに対して、Rate limitationの動作を定義します。第1段階は、リソースが`rate:with-limitation`により保護されます。

もし、特定のユーザからのblockへのアクセスが頻繁すぎる場合は、blockは呼び出されません。制限の定義があるコードが、代わりに実行されます。

`Rate limitation`は、クライアント、ユーザ、セッションごとですが、グローバルではないことに注意してください。

`rate:define-limit`, `rate:left`, `rate:with-limitation`をご参照ください。

### 2.10 サーバ

serverインターフェイスとloggerインターフェイスは、唯一、Radianceが起動時に順番通りに読み込まれるものです。
HTTPリクエストを受け入れて、応答する責任があります。
実装では、リクエストを受け入れて、Radianceの`request`関数に渡す必要があります。
その後、`response`はリクエスト主に戻されます。

リスナーの動作を特定する引数は実装によることに注意してください。
しかし、実装は、`(mconfig :radiance :port)`で設定された`localhost`とポートからでアクセスできる標準のリスナーを提供して、`radiance:startup`で起動できるようにする必要があります。

`server:start`, `server:stop`, `server:listeners`, `server:started`, `server:stopped`をご参照ください。

### 2.11 セッション
あるクライアントが行う複数のリクエストを追跡します。
クライアントによっては情報を隠蔽したり偽装している場合があるので、完全にはクライアントを追跡できるとはいえません。
しかし、多くのユーザに対しては、うまく動作するはずです。
セッションインターフェイスは、他のインターフェイスや低レイヤーのライブラリの中で使われます。ユーザ認証にような一貫性を保つために使われます。

`session:*default-timeout*`, `session:session`, `session:=`, `session:start`, `session:get`, `session:list`, `session:id`, `session:field`, `session:timeout`, `session:end`, `session:active-p`, `session:create`をご参照ください。

### 2.12 ユーザ
ユーザオブジェクトを永続させ、パーミションの仕組みを組み込めます。
ユーザ認証、ユーザの特定、トラッキング等は扱いません。
このインターフェイスでは、ユーザオブジェクトを提供するのみであり、パーミション情報が管理されます。

パーミションに関する詳細は、`user:user`を参照してください。

`user:condition`, `user:not-found`, `user:user`, `user:=`, `user:list`, `user:get`, `user:username`, `user:fields`, `user:field`, `user:remove-field`, `user:remove`, `user:check`, `user:grant`, `user:revoke`, `user:add-default-permissions`, `user:create`, `user:remove`, `user:action`, `user:ready`, `user:unready`もご覧ください。

## 参考

* [modularize](https://shinmera.github.io/modularize) パッケージのメタシステム
* [modularize-interfaces](https://shinmera.github.io/modularize-interfaces) インターフェイスと実装の拡張
* [modularize-hooks](https://shinmera.github.io/modularize-hooks) フックとトリガーの仕組み
* [ubiquitous](https://shinmera.github.io/ubiquitous) 環境設定
* [radiance-contribs](https://shirakumo.org/projects/radiance-contribs) インターフェイスの実装、他便利な機能
