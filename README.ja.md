## Radianceについて

Radianceは、Webアプリケーションの開発環境です。Webフレームワークのようなものですが、より汎用的で、自由にカスタマイズできます。特別な変更を加える必要なく、個人的なWebサイトやWebアプリケーションを簡単に書くことができます。

## 入手方法

Radiance本体、関連するモジュールやアプリケーションは、Quicklispを通して配布されています。Radianceをインストールするには、次のようにします。

```lisp
(ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
(ql:quickload :radiance)
```

以上で、Quicklispの`quickload`コマンドで[Purplish](https://github.com/Shirakumo/purplish)などのRadianceモジュールを利用できるようになります。

## チュートリアル

Radianceのチュートリアルは[こちら](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%200.md)です。チュートリアルを通して、Radianceの使い方に慣れることができます。Radianceの重要な概念やWebアプリケーションの書き方を紹介しています。特定の機能が必要なときに、どこを調べれば良いかが分かるようにしました。チュートリアルの最後では、Radianceの本番環境でのインストールとデプロイ方法について説明しています。

## 簡単な例

あなたが一番したいことはユーザにHTMLを配信することでしょう。その方向で進めながら、少しずつ拡張していきます。まずはRadianceの準備をします。

```common-lisp
(ql:quickload :radiance)
(radiance:startup)
```

初めてRadianceを使う場合には、`r-welcome`モジュールを使ってください。`r-welcome`モジュールを使うと、Welcomeページへのリンクを取得してブラウザで開くことができます。まずは小さなページにリンクをはります。

```common-lisp
(in-package :rad-user)

(define-page example "/example" ()
  (setf (content-type *response*) "text/plain") 
  "Hi!")
```

[localhost:8080/example](http://localhost:8080/example)にアクセスすると、"Hi"と表示されるはずです。これではかなり退屈ですね。次は[cl-who](http://weitz.de/cl-who/)を用いてHTMLを出してみましょう。

```common-lisp
(ql:quickload :cl-who)
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page"))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

普通であれば、ここからフォントの書式を変えたりCSSを追加しながらページのデザインを作っていくかと思います。HTMLにCSSを読み込ませることは可能ですが、長い目でみれば最良の方法とはいえません。

ここでは、モジュール(module)の作り方を紹介します。モジュールを使うことで、Webページを適切に整理することができます。モジュールに必要なファイルは手動で作れますが、ここでは雛形を用いてモジュールを自動生成します。

```common-lisp
(create-module "example")
```

モジュールは自動生成されると、モジュールへのパスが返されます。生成されるものは、ASDFのシステムファイル、Lispのmainファイル、2つのフォルダ(`static`と`template`)です。

`static`フォルダには、静的に配信されるファイルが入ります。`template`には、テンプレートエンジン関連のファイルが入ります。

`example.lisp`を開いて、先ほどのコードを元にページを定義しましょう。

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page"))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

ページはシンボルの名前で特定されます。独自にモジュールを作成すると、新しいパッケージが生成されます。上の例において、シンボル`example`は一度も使われていないシンボルです。パッケージ内での名前衝突を避けるために、`rad-userパッケージ`でページを消す必要があるかもしれません。

```common-lisp
(remove-page 'rad-user::example)
```

次に簡単なCSSファイルを作りましょう。`static`フォルダに`example.css`という名前で作成してください。CSSを書くのが面倒であれば、次のCSSコードを使ってください。

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

HTMLがこのCSSファイルにリンクするように修正しましょう。後では、アドレスをCSSファイルに対応づけるために、Radianceのルーティングシステムを利用しますが、ここでは利用せずに進めていきます。

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

ページを再度読み込むと、スタイルが適用されているはずです。`uri-to-url`が動作する原理については、後で詳しく説明します。大切な点は、どのようにセットアップしても、静的ファイルへのリンクが適切に処理されることです。

## 1. Radianceの概念と要素(Radiance Concepts & Parts)

### 1.1 URI

`URI`はRadianceの中心的な概念です。URIはオブジェクトであり、`ドメイン`、`ポート番号(オプション)`、`パス`を含むリストです。

RadianceのURIは、URIから一般的な要素を抽出したもので、スキーマ、クエリ、フラグメント等は含みません。`domains`のURIは、フレームワークにおいて複数の場所で使われる点が普通と違います。URIは`location`を捕捉したり`dispatch matching`を処理するために使われます。

URIは変更可能です。ただし、URIの修正は重要なパスに依存する場所で行われるはずなので、パフォーマンスの点で重要です。また、想定外の方法でURIを修正した場合、予想外の動作を引き起こす可能性があります。

URIは固有の文字列で表現されて、文字列にシリアライズすることができます。また、文字列をパーズして完全なURIオブジェクトに戻すことも可能です。URIはFASLファイルにリテラルで書き出されるので、マクロから吐き出すことも可能です。URIのシンタックスは、次の通りです。

```BNF
URI     ::= DOMAINS? (':' PORT)? '/' PATH?
DOMAINS ::= DOMAIN ('.' DOMAIN)*
DOMAIN  ::= ('a'..'Z' | '0'..'9' | '-')
PORT    ::= ('0'..'9'){1, 5}
PATH    ::= .*
```

URIをURLに変換するには`uri-to-url`を使います。`uri-to-url`を使うと、リバース(reversal)、エンコーディング、フォーマットの修正は自動で行われます。

`uri`, `domains`, `port`, `path`, `matcher`, `uri-string`, `make-uri`, `make-url`, `ensure-uri`, `copy-uri`, `parse-uri`, `uri<`, `uri>`, `uri=`, `uri-matches`, `merge-uris`, `represent-uri`, `uri-to-url`を参考にしてください。

### 1.2 リクエストとレスポンス(Request and Response)

サーバを行き来するデータを格納するには、`request`オブジェクトと`response`オブジェクトを使います。`request`オブジェクトは、リクエストがどこに向かうのかを表すURI、ヘッダ、POSTやGETのペイロード、クッキーなど、全ての情報を保持します。また、`response`オブジェクトは、リターンコード、ヘッダー、クッキー、BODYのデータを保持します。

リクエストが行われている間、これらの2つのオブジェクトは必ず存在し、`*request*`と`*response*`に束縛されます。それらは、動的なページ生成のために必要な多くの情報を含みます。`*request*`には`data`テーブルがあり、任意のデータを保存することができます。これは、システム内の部品の間でリクエストの実行中に取得される情報を扱うときに役立ちます。

リクエストは、必ずしもHTTPサーバから来る必要はありません。動作をテストするために、プログラムからリクエストを送ることも可能です。どのような場合でも、リクエストをディスパッチするインターフェイスは`request`と呼ばれます。この仕組みは、リクエストとレスポンスを構築して、URIを適切に処理します。もし独自にリクエストオブジェクトを送る場合は`execute-request`を使うこともできます。

リクエスト処理に関する詳しい情報は、dispatcher、pages、API endpointをご参照ください。

`*request*`, `*response*`, `*default-external-format*`, `*default-content-type*  `, `request`, `uri`, `http-method`, `headers`, `post-data`, `get-data`, `cookies`, `user-agent`, `referer`, `domain`, `remote`, `data`, `issue-time`, `response`, `data`, `return-code`, `content-type`, `external-format`, `headers`, `cookies`, `cookie`, `name`, `value`, `domain`, `path`, `expires`, `http-only`, `secure`, `cookie-header`, `cookie`, `get-var`, `post-var`, `post/get`, `header`, `file`, `redirect`, `serve-file`, `request-run-time`, `*debugger*`, `handle-condition`, `render-error-page`, `execute-request`, `set-data`, `request`も参考にしてしてください。

### 1.3 ルート(Route)

リクエストは、ルーティングシステムを通してディスパッチされます。他のフレームワークにおいて`routes`は、どのハンドラがリクエストを処理するかを指定しますが、Radianceではその方式とは違います。Radianceにおいてルート(`route`)とは、URIを変換するフォームです。ルートは、２つの**世界**を作成して保持します。**内部の世界**と**外部の世界**です。

**内部の世界**は、Webアプリケーションが実際に動作する世界です。**外部の世界**は、HTTPサーバとWebサイトを利用するユーザが暮らす世界です。この区別は、サーバの潜在的な罠を避けてWebアプリケーションを書くために必要不可欠です。あなたは、アプリケーションを動作させるために、どのドメインやポートやパスが必要になるかを心配する必要はありません。また、Webの管理者として、システムをカスタマイズしながら壊さず運用することができます。

ルーティングには、`Mapping`と`Reversal`の2種類があります。`Mapping`は、外部の世界からくるURIを内部の世界のURIに変換します。たいていの場合、トップレベルのドメインを切り取り、サブドメインにマッピングします。一方、`Reversal`は逆のことをします。内部の世界から外部の世界へと進みます。このことは、あなたが提供するWebページが、実際に外部からアクセス可能なリソースを参照できるようにするために必要です。

ルーティングは、任意の処理を行うことができます。基本的なレベルでは、何らかの方法でURIを修正する関数です。ルーティングを使うと、強力で柔軟なシステムを構築することができます。アプリケーションの開発者として、`external-uri`か`uri-to-url`を、ページ内の全てのリンクに使うようにしてください。

`route`, `name`, `direction`, `priority`, `translator`, `route`, `remove-route`, `list-routes`, `define-route`, `define-matching-route`, `define-target-route`, `define-string-route`, `internal-uri`, `external-uri`も参考にしてください。

### 1.4 URIディスパッチャー(URI Dispatcher)

ついに、リクエストに応じてコンテンツを生成する段階まできました。URIディスパッチャーは、URIのサブクラスであり、名前、関数、優先順位を運びます。リクエストがくる度に実行される優先順位のリストがあります。リクエストのURIは、それぞれのディスパッチャーに対応します。適合する１番目のディスパッチャーの関数が実行されます。

たったこれだけです。ディスパッチャー関数は、ページの内容を提供するために、必要な値を、レスポンスオブジェクトに設定する責任があります。そのためには、レスポンスオブジェクトの`data`のフィールドに直接設定するか、関数から適切な値を返します。Radianceは、4つの型のデータ（`stream`、`pathname`、`string`、`(array (unsigned-byte 8))`）を受けとります。

もしURIディスパッチャーが明示的な優先順位の番号を持っていない場合、優先順位はURIの特異性によって決まります。どのように計算がされるかについて詳しく知りたい場合は、URIソーティング関数である`uri>`をみてください。

`uri-dispatcher`, `name`, `dispatch-function`, `priority`, `uri-dispatcher`, `remove-uri-dispatcher`, `list-uri-dispatchers`, `uri-dispatcher>`, `define-uri-dispatcher`, `dispatch`もご参照ください。

### 1.5 ページ(Page)

ページは、実際にコンテンツを提供する関数を定義するために用いられます。しかし、ページはURIディスパッチャーであり、物事を簡単にするためのマクロをいくつか含みます。注目してもらいたいのは、拡張可能なオプションです。詳しくみていきましょう。

Radianceには、デフォルトでセットアップされるページがいくつかあります。例えば、`favicon`と`robots`のページは、Radianceの`static/`ディレクトリで配信されます。本番環境でも、自分のページの配信や更新をしたいことがあるかと思います。

そのような目的を果たすために、`static`ページの仕組みがあります。`static`ページは、静的コンテンツをWebアプリケーションとモジュールに渡します。`static`ページは、そのドメインでも`/static/...`のパスで有効であり、最初のディレクトリがモジュール名の形式である必要があります。残りは、モジュールの`static/`ディレクトリの範囲にあるパスです。この仕組みにより、CSS、JavaScript、画像などの静的ファイルを参照することができます。

最後に`api`ページですが、APIエンドポイントのディスパッチを処理する役割があります。これについては次の章で説明します。ページは、静的ファイルの場合と同様に、どのドメインにおいても`/api/...`のパスを捕捉して動作します。

`page`, `remove-page`, `define-page`をご参照ください。

### 1.6 APIエンドポイント(API Endpoint)

Radianceは、REST APIとの連携もサポートしています。これは取ってつけた機能ではありません。多くの現代のアプリケーションは、そのようなAPIを提供することが多いですが、RadianceでもAPIエンドポイントを含むWebアプリケーションを書くことができます。

概念を説明すると、APIエンドポイントは、ブラウザーのリクエストに応じて呼び出すことができる関数です。レスポンスは、リクエストの送り主が読める形式にシリアライズされます。重要な点は、APIエンドポイントはユーザからもプログラムからも利用可能であるべきということです。APIを通してプログラムから実行されるアクションは全てユーザによって実行されるので、Radianceでは両方から利用可能にするように推奨しています。重複を避けるために、これら2つを1つのものとして扱います。

全てのデータの更新は、APIエンドポイントを通して行われます。リクエスト主が`ユーザ`か`プログラム`かを区別せずに処理されます。ユーザがリクエストを行う場合は、適切なページにリダイレクトします。プログラムからリクエストが行われた場合は、読み込めるフォーマットでデータのペイロードが提供されます。

全てのパートのうち、APIフォーマットのシステムですが、データを特別なフォーマットにシリアライズすることを担当します。デフォルトでは、S式を基本とするフォーマットが提供されていますが、JSON形式の出力も簡単にロードできます。

次に、`browser`のPOST/GETパラメータの仕様をみましょう。そのパラメータが`"true"`という文字列を含む場合、APIリクエストはユーザからであると扱われて、ペイロードデータは出力されずにリダイレクトされます。

あなたのアプリケーションは、統一されたAPIを提供するために、これらをうまく使う必要があります。今、実際のエンドポイントの定義は、名前、関数、引数を記述するラムダリスト、リクエストのパーズ関数で構成されます。引数の典型としては、必須の引数かオプショナル引数が理にかなっています。結局、HTTPリクエストは、**キーワード引数**しかもつことができません。**キーワード引数**は省略可能です。

APIエンドポイントの名前は、どこにリーチできるかを示す識別名として機能します。APIエンドポイントは、`/api/`パスに存在し、エンドポイントの名前もそれに従います。エンドポイントを修正する際には、あなたのモジュールやアプリケーションが、他のエンドポイントをうっかり踏み外さないように気をつけなければいけません。これはURIディスパッチのときとは違いますが、理由は、APIエンドポイントは、曖昧さやパスのプロセスを許可しないからです。なので、全てのエンドポイントは、唯一のパスをもなければいけません。唯一のパスは、直接、名前としてサーブします。

生の(raw)関数は、APIがインターフェイスを提供する関数です。生の関数は、リクエストされたアクションを行い、適切なデータを上で述べた通りに返す役割を果たします。フォーマットされたAPIのデータを返すためには、`api-output`を使います。ブラウザリクエストからリクエストを受けてリダイレクトをするには、`redirect`を使います。

最後に、リクエストのパーズ関数は、リクエストオブジェクトを受け取り、関数が必要な実引数を抽出して、最終的に、可能であれば、適切な引数を適用させて関数を呼び出します。パーズ関数は、もし必要な引数が見当たらない場合、`api-argument-missing`エラーの信号を送ります。無駄な実引数は無視されます。

`call-api`を用いると、プログラムからAPIエンドポイントを呼び出すこともできます。`call-api-request`を使うと、Requestをシュミレーションすることができます。どちらもURIディスパッチの仕組みを通る必要はありません。

ページと似ていますが、APIエンドポイントの定義は、定義を簡単にするために、拡張オプションも受け付けます。オプションについては、次で説明します。

`api`, `*default-api-format*`, `*serialize-fallback*`, `api-format`, `remove-api-format`, `list-api-formats`, `define-api-format`, `api-output`, `api-serialize`, `api-endpoint`, `remove-api-endpoint`, `list-api-endpoints`, `api-endpoint`, `name`, `handler`, `argslist`, `request-handler`, `call-api-request`, `call-api`, `define-api`をご参照ください。

### 1.7 オプション(Option)

オプションは、拡張しやすい定義用マクロを提供します。フレームワークで何か定義するときに、共通のオペレーションを短しようとするときに役に立ちます。例えば、アクセス権があるユーザに対して、ページかAPIエンドポイントを制限するような共通するタスクがあるとします。

このような実装を簡単にするために、Radianceは一般的なオプションに仕組みを提供します。オプションは、定義のマクロが属するオプションの型によって分けられます。Radianceは、`api`と`page`のオプションを提供します。

それぞれのオプションは、オプションの型に応じて、名前と多くの引数を受け入れる関数のために、キーワードをもっています。定義名、ボディー部、最終にオプションに渡される値を含んだリストが、いつも引数として与えられます。

この拡張用(expansion)の関数は、定義マクロのボディーの式を変換します。環境の設定を許可するために、定義自体では含まれない式を吐き出すこともできます。

`option`, `option-type`, `name`, `expander`, `option`, `remove-option`, `list-options`, `define-option`, `expand-options`をご参照ください。

### 1.8 モジュール(Module)

モジュールの概念は、Radianceにおいて必要不可欠です。全体を構成する**要素**として働きます。技術レベルでいうと、モジュールは、特別なメタデータが与えられたパッケージです。モジュールは、`modularize`システムによって提供され、フック、トリガー、インターフェイス等を使いやすくして、他の情報をトラッキングするために使われます。

`defpackage`を使う代わりに、`define-module`を使うようにしてください。シンタックスは`defpackage`ですが、`:domain`のような特別なオプションを含んでいるので、モジュールが機能するプライマリ・ドメインを特定することができます。

モジュールのシステムでは、ASDFのシステムをモジュールに記すこともできます。もし記せば、そのASDFシステムは**仮想のモジュール**になります。このようにするためには、システムの定義にオプションを3つ追加する必要があります:

```commonlisp
:defsystem-depends-on (:radiance)
:class "radiance:virtual-module"
:module-name "MY-MODULE"
```

こうすることで、Radianceは、ASDFのシステム情報を特定して、あなたのモジュールに関連づけることができます。新しいモジュールのために、必須のシステムとモジュールの定義を自動で行うには、`create-module`をみてください。

`virtual-module`, `virtual-module-name`, `define-module`, `define-module-extension`, `delete-module`, `module`, `module-p`, `module-storage`, `module-storage-remove`, `module-identifier`, `module-name`, `current-module`, `module-domain`, `module-permissions`, `module-dependencies`, `module-required-interfaces`, `module-required-systems`, `module-pages`, `module-api-endpoints`, `describe-module`, `find-modules-directory`, `*modules-directory*`, `create-module`をご参照ください。

### 1.9 フック(Hook)

Radianceには、互いのモジュールを連携させるために、フック(hook)の仕組みがあります。フックを使うと、ある種のイベントに反応して、任意の関数を実行できます。例えば、意見を交換するソフトウェアでは、投稿が新たに作られる度にトリガーされるフックを設定することができます。拡張では、追加のタスクを実行するためにトリガーをフックに対して定義できます。

フックは、任意の数のトリガーをもつことができますが、トリガーは長すぎないようにしてください。フックをトリガーすることは、全てのトリガーが終了するまでブロックされる動作だからです。そのようなわけで、長い間続くようなトリガーの実行は、リクエストへのレスポンスを遅らせてしまう可能性があります。

フックは、長い間は**on**であり、後に**off**になるスイッチにような関数であるべきです。また、新しいトリガーが実行中に定義された場合は、自動で呼び出されるべきです。これは`define-hook-switch`により容易になります。`define-hook-switch`は2つのフックを作ります。1つ目がトリガーされると、それに定義されているトリガーは、後に2つ目のフックがトリガーされたときに自動で呼び出されます。このおかげで、仮にトリガーがサーバの起動後に定義されたとしても、`server-start`は適切に動作します。

`list-hooks`, `define-hook`, `remove-hook`, `define-trigger`, `remove-trigger`, `trigger`, `define-hook-switch`をご参照ください。

### 1.10 インターフェイス(Interface)

システムが一枚岩になることを避けるために、またバックエンドも拡張できるように、Radianceには`インターフェイス(Interface)`の仕組みがあります。一般的にインターフェイスとは、関数、マクロ、変数がどのように動作するかについて約束事を書くものであり、実際の実装をそこに書かないことをいいます。つまり。インターフェイスがうまく動作するためには、実装を外側に置くことが大切です。このことにより、インターフェイスに対してコードを書くことができ、特定のバックエンドに結びつけることなく、与えられた機能を利用することができます。

具体例として、データベースのためのインターフェイスをみていきましょう。データベースには多くの種類があり、全ては違う方法でやりとりをしますが、データの保存、検索、修正等、どれにも共通する操作があるので、インターフェイスは実用的です。では、共通する操作を提供するインターフェイスを作ります。これは、特定の種類のデータベースが実際にすることは、それぞれの実装次第です。また、アプリケーションの作者として、データベース・インターフェイスを活用することができます。このインターフェイスを使えば、様々なデータベースに対して自動的にうまく動作するようにできるでしょう。

これはアプリケーションの作者に有利なだけではありません。インターフェイスで実装を分離することにより、システム管理者は、比較的容易に、特殊な機能を独自に実装することができます。インターフェイスが不透明であることにより、Lispのプロセスで動くものと、外部で動くプロセスを連携することができます。これは、Productionシステムの管理者が必要な情報を選び出すために、開かれた選択肢を多く与えます。

実際、インターフェイスは特別な種類のモジュールであり、特別な種類のパッケージです。定義の一部として、関数や変数などの束縛のために、一連の定義を含みます。インターフェイスはパッケージなので、あなたがコンポーテントで使えるユーザは、他のパケージにあるものも何でも使えます。違いはありません。実装の作者として、あなたはインターフェイスが示す定義を再定義することができます。

実際にインターフェイスを利用するモジュールを読み込むためには、インターフェイスの実装は、事前に読み込まれる必要があります。そうでなければ、マクロは適切に動作しません。あなたのASDFシステムの定義において、特定の実装を参照する必要なく、インターフェイスに依存することを許可するためには、Radianceは拡張されたASDFを提供します。この拡張を使うと、`(:interface :foo)`のようなリストを`:depends-on`に追加することができます。モジュールがロードされたときに、Radianceはインターフェイスを具体的な処理に分解します。

Radianceは、標準のインターフェイスを提供します。それぞれのインターフェイスは、[radiance-contribs](https://shirakumo.org/projects/radiance-contribs)によって提供される標準実装を1つ以上もちます。インターフェイスは次の通りです:

* `admin`

　　拡張可能な管理者ページを提供します。

* `auth`

　　認証とログインに関することを全て処理します。

* `ban`

　　IPアドレスによってユーザがサイトにアクセスすることを禁止します。

* `cache`

　　キャッシュの仕組みを提供します。

* `database`

　　データベースの柔軟なインターフェイスです。オブジェクトの保存、リレーショナルデータベースをバックエンドとして利用できます。

* `logger`

　　ログ出力のためのインターフェイスです。デバッグやメッセージを出力できます。

* `mail`

　　メールを送るための最小限のインターフェイスです。

* `profile`

　　ユーザのプロフィールや属性を拡張するために使います。

* `rate`

　　特定のリソースにrate limitationを許可します。

* `server`

　　Radianceを外部の世界と結びつける架け橋のような役割を果たすインターフェイスです。

* `session`

　　トラッキングするために、継続するセッションを保証します。

* `user`

　　ユーザアカウントとパーミションの機能を提供します。

それぞれのインターフェイスについては、次の章で説明します。

`interface`, `interface-p`, `implementation`, `implements`, `reset-interface`, `define-interface-extension`, `find-implementation`, `load-implementation`, `define-interface`, `define-implement-trigger`をご参照ください。

### 1.11 環境(Environment)

同じマシン内の複数のRadianceインスタンスに対して異なる設定ができるように、Radianceでは環境(Environment)という仕組みを用意しています。基本的に環境とは、Radianceの設定、ランタイム・ファイル、読み込まれるモジュールの一式です。Radianceの設定は、インターフェイスの実装へのマッピングを含み、インターフェイスが求められたときに選ばれるようにします。

`startup`が呼び出された時、どれだけ遅くとも、特定の環境が選択されます。早ければ、モジュールがロードされたときに選択されます。後者の場合は、環境を選ぶために、インタラクティブな再起動が可能です。これは必須の機能ですが、理由は、そうでなければ、Radianceがインターフェイスのマッピングを解決できないからです。

環境のシステムの一部として、Radianceは、あなたのアプリケーションで使える(使うべき)設定システムを提供します。設定システムを使うと、それぞれの環境ごとに、適切に設定することができます。その設定は、いつでも持続性があるうえに可読性にも優れたフォーマットで保存されるので、特別なツールで読み込んだり修正したりする必要はありません。

実際に設定手順でどのように保存処理がされているかを知りたい方は、[ubiquitous](https://shinmera.github.io/ubiquitous)を参考にしてください。 `value`関数の代わりに、Radianceでは`config`関数を使えます。

設定ファイルとは別に、環境は、ユーザのアップロードやキャッシュなどのランタイムのデータのために、継続的な記憶場所を提供します。`environment-module-directory`と`environment-module-pathname`を使うことで、この場所から情報を取り出すことができます。アップロードやキャッシュを保存するとき、モジュールはこれらのパス(path)を利用して、継続的なインターフェイスを管理者に与えます。デプロイされたシステムでは、環境の記憶場所を変更することが望ましいかもしれません。そのような場合、管理者は、想定通りに動作させるために`environment-directory`と `environment-module-directory`の上で新たなメソッドを与えることをおすすめします。詳しい動作については、関連するドキュメントを参照してください。

環境は、管理者の上書きを許可します。`environment-module-directory`に`:static`と`:template`を使うことで、モジュールの標準テンプレートや静的なファイルへのパスをえることができます。各々のディレクトリ中でのパスは、モジュール自体のソースファイルと対応する必要があります。

実際にモジュールが利用する静的ファイルとテンプレートは、ロード時にキャッシュされることに注意してください。したがって、Lispイメージが再起動されるか、モジュールのソースファイルが再読みされない限り、変更は有効になりません。

`environment-change`, `environment`, `environment-directory`, `environment-module-directory`, `environment-module-pathname`, `check-environment`, `mconfig-pathname`, `mconfig-storage`, `mconfig`, `defaulted-mconfig`, `config`, `defaulted-config`, `template-file`, `@template`, `static-file`, `@static`をご参照ください。

### 1.12 マイグレーション・システム(Migration System)

システムは、以前のバージョンと互換性のない形で進化することがあります。そのような場合、現在のセットアップが、新しいバージョンと機能的に問題なく動作するように、ランタイム・データのマイグレーションが必要です。Radianceは、このプロセスを自動化して、アップグレードを円滑に進める仕組みがあります。
  　
異なるバージョン間でのマイグレーションは、Radianceが起動する際に自動で行われるべきです。管理者として、マイグレーションが適切にされるために、何か他に行う必要はありません。しかしながら、モジュールの作者として、モジュールに対してデータのマイグレーションがされるために実行されるコードを提供する必要はあります。

モジュールをマイグレーションできるように、バージョンの仕様をもっているASDFによりモジュールを読み込む必要があります。そのバージョンは、標準的なドットで区切られた数字の形式をとります。オプションとして、ハッシュのバージョンを語尾にたすことができます。

その後、`define-version-migration`で、個々のバージョン間でのマイグレーションのステップを定義することができます。一度定義されると、Radianceは、自動的に固定のバージョンを選び、現在のバージョンに合うように、必要なマイグレーションの手順を実行します。マイグレーションの手順に関するより詳しい情報は、`migrate`と`migrate-versions`をご覧ください。

`last-known-system-version`, `migrate-versions`, `define-version-migration`, `ready-dependency-for-migration`, `ensure-dependencies-ready`, `versions`, `migrate`も参照ください。
  
### 1.13 インスタンス管理(Instance Management)

Radianceは、ソフトウェアの起動から終了までの一連の流れを提供します。これによりソフトウェアは、適切に起動して利用可能になり、綺麗に片付けて終了することを保証します。一連の流れの多くは、決まったフックが、正しい順序で、適切な回数だけ呼ばれることにより実現されています。

インターフェイスの関数を適切に使うことで、サーバを手動で起動することは可能ですが、手動でアプリケーションが正しく動作すると想定しないでください。多くの場合、特定のフックが適切な順番で呼び出される必要があるので、`startup`と`shutdown`によりRadianceのインスタンスを管理する必要があります。`startup`と`shutdown`のドキュメントには、どのフックが、どの順番で呼び出されているかが説明されています。実装では、シンボルがexportされていない限り、追加で特定されない定義をインターフェオスのパッケージのシンボルに加えることができます。

`*startup-time*`, `uptime`, `server-start`, `server-ready`, `server-stop`, `server-shutdown`, `startup`, `startup-done`, `shutdown`, `shutdown-done`, `started-p`をご参照ください。

## 2. 標準インターフェイス

標準のインターフェイスは、Radianceとcore packageと一緒に配布されています。ライブラリは、追加のインターフェイスを提供することも可能です。インターフェイスの実装ですが、インターフェイス定義では、次の制限の緩和が許可されています:

`&key`引数を含むラムダリストは、実装依存のキーワード引数を使って拡張できます。`&optional`引数を含み、`&key`か`&rest`を含まないラムダリストは、オプショナル引数で拡張できます。必須の引数しか含まないラムダリストは、オプショナル引数かキーワード引数で拡張できます。

### 2.1 管理(admin)

adminインターフェイスは、管理者ページを作成するためのものです。ユーザ構成の設定やシステム情報の表示のために使えます。"管理(administration)"という名前ですが、システムの管理者だけに限ったものではありません。どのユーザにも利用できることができます。

管理者ページは、カテゴリー分けされたメニュやパネルを表示できなければいけません。パネル群は、他のモジュールによって提供されており、`admin:define-panel`で追加できます。秘密情報を含むようなパネルにアクセスするパネルは、`:access`オプションでアクセスを禁止させるようにしてください。パーミッションについては、 userインターフェイスを参照してください。

管理者ページや特定のパネルにリンクをはるには、`page`リソースを使ってください。

`admin:list-panels`, `admin:remove-panel`, `admin:define-panel`, `admin:panel`をご覧ください。

### 2.2 認証(auth)

authインターフェイスは、ユーザをリクエストと結びつけます。そのために、ユーザがシステムに対して自分自身を認証させる方法を提供しなければいけません。どのように実現するかは、実装次第です。実装は、認証のプロセスが初期化するためのページを提供しなければいけません。`page`のソースを通してURIを認証プロセスに渡して、`"login"`を引数として渡します。

現在リクエストに結び付けられているユーザを`auth:current`コマンドで調べることができます。ユーザが`"anonymous"`と解釈される場合は、`NIL`を返します。詳しくはuserインターフェイスを参照ください。

`auth:*login-timeout*`, `auth:page`, `auth:current`, `auth:associate`もご覧ください。

### 2.3 禁止(ban)

banインターフェイスは、IPによるアクセス制限を提供します。IP BANされたクライアントのIPアドレスからは、リクエストするページに対してアクセス出来なくなります。BANは、タイムアウトの後、手動と自動いずれでも、離す(lift)することができます。実装として、ユーザのIPを監視するために、追加の労力を割くことは想定していません。

`ban:jail`, `ban:list`, `ban:jail-time`, `ban:release`をご参照ください。

### 2.4 キャッシュ(cache)

cacheインターフェイスは、一般的なキャッシュの仕組みを提供します。カスタマイズ可能で無効化のテストをもっています。キャッシュを明示的に新しくするには、`cache:renew`をつかいます。`cache:with-cache`を使うと、キャッシュの部品を構築することができ、テストのformがtrueと評価されたときに、BODYのキャッシュ値が返されるようにできます。

キャッシュ値が保存される方法は実装によります。 `cache:get`か`cache:with-cache`を使うと、キャッシュ値は文字列かバイト列に強制変換されます。実装では、キャッシュに格納できる　typeには制限がありませんが、少なくとも、文字列とバイト列はサポートします。

キャッシュ値の名前は、名前とパッケージ名が、次の文字を含まないシンボルでなければいけません:`<>:"/\|?*.` 
キャッシュ値の変形は、`princ`によって表示される表現とは区別されるオブジェクトである必要があります。その際には、先ほどと同じ文字の制限が適用されます。

`cache:get`, `cache:renew`, `cache:with-cache`をご参照ください。

### 2.5 データベース(database)

databaseインターフェイスは、データを持続させるためのレイヤーを提供します。通常は、データベースと呼ばれるレイヤーです。リレーショナル型のデータベースである必要はありませんが、そうであってもいいです。実装の変数を保持するために、基本的なデータベースの機能しかサポートされません。(joinsやtriggers等はありません)データ型も、整数、float、文字列に限定されます。これらの制限にも関わらず、多くのアプリケーションにおいて、データベースインターフェイスはとても役に立ちます。

伝統的なRDMBの用語と区別するために、特別な用語が使われます。`schema`は"structure"、`table`は"collection"、`row`は"record"とします。

データベースに接続する前に、データベース関連の命令を実行すると、未定義の動作を招きます。Radianceでは、Radianceが動作している間はデータベースが接続されることを保障しているので、どのページ、どのAPI、どのURIディスパッチャーの定義において、データベースインターフェイスを問題なく使えます。

実際にデータ保存を行うための関数は、`db:insert`、`db:remove`、`db:update`、`db:select`、`db:iterate`です。
それらは、あなたが期待するように動作するはずです。

詳しくは、それぞれの関数に書かれているコメントを読んでください。コレクションの作り方、どのような制限があるかを知りたい方は、`db:create`のコメントも参考にしてください。

データベースは、データ操作が完了すれば、Radianceを再起動したり、Lispのイメージや機械がクラッシュしたとしても、データの変更は存続されなければいけません。

`database:condition`, `database:connection-failed`, `database:connection-already-open`, `database:collection-condition`, `database:invalid-collection`, `database:collection-already-exists`, `database:invalid-field`, `database:id`, `database:ensure-id`, `database:connect`, `database:disconnect`, `database:connected-p`, `database:collections`, `database:collection-exists-p`, `database:create`, `database:structure`, `database:empty`, `database:drop`, `database:iterate`, `database:select`, `database:count`, `database:insert`, `database:remove`, `database:update`, `database:with-transaction`, `database:query`, `database:connected`, `database:disconnected`を参照してください。

### 2.6 ロガー(logger)

loggerインターフェースは、ログの関数を提供します。システムの中で関連して起きていることについて、ログのメッセージを出すことができます。ログの出力内容と出力方法に関しては、実装とシステムの管理者次第です。

`logger:log`, `logger:trace`, `logger:debug`, `logger:info`, `logger:warn`, `logger:error`, `logger:severe`, `logger:fatal`をご覧ください。

### 2.7 メール(mail)

mailインターフェースは、メールを送る仕組みを組み込むことができます。様々なコンポーネントが、Webサイトの外からユーザとつながるために、メールのアクセスが必要になるかもしれません。リモートサーバ、ローカル環境でのメール送信等、メールの送信方法の設定は、処理系依存です。`mail:send`のフックを使うと、メールが送られる前に、メールに反応することができます。

`mail:send`をご覧ください。

### 2.8 プロフィール(profile)

profileインターフェイスを使うと、ユーザが存在するアプリケーションにおいて、ユーザへのインターフェイスを拡張することができます。このインターフェイスは、機能の一部として、ユーザの**プロフィール**が表示されるページを提供する必要があります。その**プロフィール**は、数種類のパネルを表示しなければいけません。パネルは、他のモジュールによって提供されており、`profile:define-panel`で追加できます。

`page`リソースの型情報により、ユーザのプロフィールページへのURIを得ることができます。

このインターフェイスを使うと、視覚的にユーザを特定させるために、`profile:avatar`で**アバター画像**にアクセスすることができます。また、`profile:name`を使うとユーザ名をカスタマイズできます。`profile:fields`、`profile:add-field`、`profile:remove-field`を使うと、どのようなデータをユーザの属性に含むか、それを公に(public)表示させるかどうか指定できます。

`profile:page`, `profile:avatar`, `profile:name`, `profile:fields`, `profile:add-field`, `profile:remove-field`, `profile:list-panels`, `profile:remove-panel`, `profile:define-panel`, `profile:panel`をご参照ください。

### 2.9 評価(rate)

rateインターフェースは、[Rate limitation](https://en.wikipedia.org/wiki/Rate_limiting)の仕組みを提供します。秘密情報や負荷が高いリソースへのアクセスを防ぐことができます。２つの段階があります。第1段階は、`rate:define-limit`により特定のリソースに対してRate limitationの動作を定義します。その後、リソースが`rate:with-limitation`マクロにより、リソースが保護されます。もし、特定のユーザからblockへのアクセスがあまりに頻繁にされる場合、ブロック(block)は呼び出されません。制限定義内のコード(the code in the limit definition)が、代わりに実行されます。

`Rate limitation`は、各クライアント、各ユーザ、各セッションごとで実装に依存しますが、グローバルではないことに注意してください。

`rate:define-limit`, `rate:left`, `rate:with-limitation`をご参照ください。

### 2.10 サーバ(server)

serverインターフェイスとloggerインターフェイスは、Radianceが起動時に順番通りに読み込まれます。HTTPリクエストに応答する責任があります。実装では、リクエストを受け入れて、Radianceの`request`関数に渡す必要があります。その後、`response`はリクエスト主に戻されます。

リスナーの動作を特定する引数は実装によることに注意してください。しかし、実装は、`(mconfig :radiance :port)`で設定された`localhost`とポートからでアクセスできる標準のリスナーを提供して、`radiance:startup`で起動できるようにする必要があります。

`server:start`, `server:stop`, `server:listeners`, `server:started`, `server:stopped`をご参照ください。

### 2.11 セッション(session)

sessionインターフェースは、あるクライアントが行う複数のリクエストを追跡します。クライアントによっては情報を隠蔽したり偽装している場合があるので、完全にはクライアントを追跡できるとはいえません。しかし、多くのユーザに対しては、うまく動作するはずです。セッション・インターフェイスは、他のインターフェイスや低レイヤーのライブラリの中で使われます。ユーザ認証にような一貫性を保つために使われます。

`session:*default-timeout*`, `session:session`, `session:=`, `session:start`, `session:get`, `session:list`, `session:id`, `session:field`, `session:timeout`, `session:end`, `session:active-p`, `session:create`をご参照ください。

### 2.12 ユーザ(user)

userインターフェースは、ユーザオブジェクトを永続させ、パーミションの仕組みを組み込めます。ユーザ認証、ユーザの特定、トラッキング等は扱いません。このインターフェイスでは、ユーザオブジェクトを提供するのみであり、パーミション情報が管理されます。

パーミションに関する詳細は、`user:user`を参照してください。

`user:condition`, `user:not-found`, `user:user`, `user:=`, `user:list`, `user:get`, `user:id`, `user:username`, `user:fields`, `user:field`, `user:remove-field`, `user:remove`, `user:check`, `user:grant`, `user:revoke`, `user:add-default-permissions`, `user:create`, `user:remove`, `user:action`, `user:ready`, `user:unready`もご覧ください。

## バージョンの変更

### 1.0 -> 2.0

* issue [#28](https://github.com/Shirakumo/radiance/issues/28)のなかで、`*environment-root*`は削除されました。[#29](https://github.com/Shirakumo/radiance/pull/29)で問題点が修正されました。`*environment-root*`は、`environment-directory`関数に統合されることにより、より汎用的なメカニズムに置き換えられました。もし`*environment-root*`をカスタマイズして利用していた場合、`§1.11`を参考に`environment-directory`を変更してください。

* issue [#30](https://github.com/Shirakumo/radiance/issues/30)で、モジュールのバージョンを変更するために、自動でマイグレーションができる仕組みを入れました。[#31](https://github.com/Shirakumo/radiance/issues/31)で修正を加えました。この機能は、すでにRediance自体とモジュールで、新しい環境ディレクトリ(environment directories)を構成するために使われており、自動で以前のデータを現在の新たな場所に移動してくれます。詳しくは`§1.12`を参考にしてください。

* issue [#26](https://github.com/Shirakumo/radiance/issues/26)の後、モジュールのソースを変更することなく、管理者がモジュールのテンプレートや静的ファイルを上書きできるようになりました。環境に関連するドキュメントは、`§1.11`を参照してください。

* ユーザ・インターフェイスは、整数のuser:idが、各々のユーザ・オブジェクトをサポートすることを求められます。これにより、データベースでユーザを参照できて、より効率的に記録できるようになりました。

* データベース・インターフェイスは、`:unique`、`db:select`、`db:iterate`のサポートを求められます。

## 参考

* [modularize](https://shinmera.github.io/modularize) ：パッケージのメタシステム
* [modularize-interfaces](https://shinmera.github.io/modularize-interfaces) ：インターフェイスと実装の拡張
* [modularize-hooks](https://shinmera.github.io/modularize-hooks) ：フックとトリガーの仕組み
* [ubiquitous](https://shinmera.github.io/ubiquitous)： 環境設定
* [radiance-contribs](https://shirakumo.org/projects/radiance-contribs) ：インターフェイスの実装、他便利な機能
